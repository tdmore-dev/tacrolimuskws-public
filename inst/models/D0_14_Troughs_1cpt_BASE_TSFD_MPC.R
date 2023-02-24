library(tdmore)
library(magrittr)

#'
#' Build 'D0_14_Troughs_1cpt_BASE_TSFD_MPC' model.
#'
#' @return a tdmore model ready for MPC
#'
buildModel <- function() {
  populationParameters <- "parameter,value,se_lin,rse_lin
V_pop,0.881282394440027,0.0267168396732024,3.03158667888498
k0_pop,0.00584520007527414,0.000651721699374156,11.149690189922
kplus_pop,0.0256441923377189,0.000751539033189455,2.93064029193093
kk_pop,0.0358955600794432,0.00360090012073341,10.0316031084735
omega_V,0.454520674593762,0.0249221688168292,5.48317605994577
omega_k0,0.728634250214592,0.134595080727694,18.4722418261363
omega_kplus,0.437984911808295,0.0249597551626306,5.69877054887119
omega_kk,1.51685096934881,0.0812187676237852,5.35443291826176
corr1_k0_V,0.0452571111009872,0.180842047310432,399.588137446288
corr1_kk_V,0.0757299090521244,0.0768053066626737,101.420043446519
corr1_kplus_V,-0.385809009312755,0.0709623673908038,18.3931338247413
corr1_kk_k0,-0.533610347912404,0.127004108533948,23.8009081028534
corr1_kplus_k0,-0.199044664387166,0.175065509160619,87.9528771593171
corr1_kplus_kk,-0.4364544512759,0.0657531869463435,15.0653033218301
a,0.0125709661320684,0.100796056016528,801.816303994318
b,0.17921378237182,0.0088165277259459,4.91955898104643"
  
  monolixValues <-
    utils::read.csv(text = populationParameters) %>%
    dplyr::mutate(
      OMEGA=stringr::str_detect(parameter,"^omega"),
      GAMMA=stringr::str_detect(parameter, "^gamma"),
      CORR=stringr::str_detect(parameter, "^corr"),
      THETA=stringr::str_detect(parameter, "_pop$")
    )
  
  thetaValues = monolixValues %>% 
    dplyr::filter(!OMEGA & !GAMMA & !CORR) %>%
    dplyr::mutate(text=paste0(parameter, "=", value)) %>% 
    dplyr::pull(text) %>% 
    paste(collapse="\n")
  m1Code <- thetaValues %>% paste("
TSFD = t;
TSFD2 = t;

V_pop_next <- exp(log(V_pop) + eta_TxNR_V );
k0_pop_next <- exp(log(k0_pop) + eta_TxNR_k0 );
kplus_pop_next <- exp(log(kplus_pop) + eta_TxNR_kplus );
kk_pop_next <- exp(log(kk_pop) + eta_TxNR_kk );

V = V_pop_next;
k0 = k0_pop_next;
kplus = kplus_pop_next;
kk = kk_pop_next;


k= k0+kplus*(1-exp(-kk*TSFD2));

d/dt(A1) = -k*A1;
Cwb = A1 / V;
  ")
  
  #message("Loading OMEGA matrix")
  omegaMonolix <- monolixValues %>% dplyr::filter(OMEGA | GAMMA) %>% dplyr::mutate(
    parameterName = substr(parameter, 7, 999),
    etaName = ifelse(OMEGA, 
                     paste0("eta_TxNR_", parameterName),
                     paste0("eta_OCC_", parameterName)
    )
  )
  omega <- diag( omegaMonolix$value**2 )
  colnames(omega) <- omegaMonolix$etaName
  rownames(omega) <- omegaMonolix$etaName
  
  #message("Loading Correlations")
  corrMonolix <- monolixValues %>% dplyr::filter(CORR) %>% 
    tidyr::extract(parameter, regex="^corr(\\d*)_(.*)_(.*)$", into=c("i", "par1", "par2"))
  for(i in seq_len(nrow(corrMonolix))) {
    par1 <- omegaMonolix %>% dplyr::filter(parameterName == corrMonolix$par1[i] ) %>%
      dplyr::filter(dplyr::row_number() == corrMonolix$i[i] )
    par2 <- omegaMonolix %>% dplyr::filter(parameterName == corrMonolix$par2[i] ) %>%
      dplyr::filter(dplyr::row_number() == corrMonolix$i[i] )
    covValue <- corrMonolix$value[i] * par1$value * par2$value
    omega[ par1$etaName, par2$etaName ] <- covValue
    omega[ par2$etaName, par1$etaName ] <- covValue
  }
  
  thetaDf <- monolixValues %>% dplyr::filter(THETA)
  theta <- thetaDf$value
  names(theta) <- thetaDf$parameter
  
  RxODE::RxODE(m1Code) %>% tdmore(
    parameters=rownames(omega),
    omega=omega,
    iov=rownames(omega), 
    res_var=list( errorModel(var="Cwb", 
                             add=monolixValues %>% dplyr::filter(parameter=="a") %>% dplyr::pull(value), 
                             prop=monolixValues %>% dplyr::filter(parameter=="b") %>% dplyr::pull(value)) 
    )
  ) %>% metadata(output(name="Cwb", label="Tacrolimus concentration", unit="ng/mL", default_value=5)) %>%
    metadata(formulation(name="PROGRAFT", unit="mg", dosing_interval=12, default_value=5, round_function=function(x){
      x <- round(x/0.5)*0.5
      x[x > 20] <- 20
      x
      })) %>%
    metadata(formulation(name="ADVAGRAF", unit="mg", dosing_interval=24, default_value=5, round_function=function(x){
      x <- round(x/0.5)*0.5
      x[x > 40] <- 40
      x
      })) %>%
    metadata(target(min=12, max=15)) %>% #mpc(theta=theta, suffix="_next") %>%
    mpc() %>%
    metadata(observed_variables(c("V", "k0", "kplus", "kk")))
}
buildModel()
