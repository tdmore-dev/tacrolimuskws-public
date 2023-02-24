## To run this, use 'docker run -p 8080:8080 -p 8081:8081 -e API_KEY="myKey" --rm -it rfaelens/tacrolimuskws'

ARG DOCKER_TAG=latest

# Use the pre-built shinytdmore image
FROM rfaelens/shinytdmore:$DOCKER_TAG

# Set the working directory to /app
WORKDIR /app/tacrolimuskws

RUN apt-get update && apt-get install -y libsodium-dev

## Initialize renv, see https://environments.rstudio.com/docker#r-packages
COPY renv.lock .
RUN R -e 'renv::restore(library=.libPaths()[1])'

## Ensure all required packages were installed through renv
COPY DESCRIPTION .
RUN R -e 'a <- remotes::local_package_deps(dependencies=TRUE); b<-rownames( installed.packages() ); if(!all(a %in% b)) stop(setdiff(a, b))'

## Install full package
COPY . .
RUN R -e 'devtools::install(dependencies=FALSE)'

# Make port 8080 and 8081 available to the world outside this container
EXPOSE 8080
EXPOSE 8081

# Run supervisor for three processes
CMD ["Rscript", "inst/launch.R"]
