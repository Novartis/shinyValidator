update_docker_image <- function(v = desc::desc_get_version()) {
  system(
    sprintf(
      "docker build -t divadnojnarg/shinyvalidator-docker:v%s .
      docker push divadnojnarg/shinyvalidator-docker:v%s",
      v,
      v
    )
  )
}

update_docker_image()
