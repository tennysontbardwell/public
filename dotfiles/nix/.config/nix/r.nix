{ pkgs, ... }:
let
  myRPackages = with pkgs.rPackages; [
    dplyr
    forcats
    ggplot2
    htmlwidgets
    purrr
    readr
    stringr
    tibble
    tidyr
    xts
    ggridges
    viridis
    hrbrthemes
    GGally
    AER
    r2d3
    dygraphs
    leaflet
    plotly
    # rbokeh
    visNetwork
    DT
    threejs
    networkD3
    tidyAML
    vvtableau
    WeightedTreemaps
    corrr
    caret
    lime
    h2o
    recipes
    tidymodels
    janitor
  ];

  myREnv = pkgs.rWrapper.override{
    packages = myRPackages;
  };

  myRStudio = pkgs.rstudioWrapper.override{
    packages = myRPackages;
  };

in
{
  # environment.systemPackages = [
  #   myREnv
  #   myRStudio
  # ];

  # packages."aarch64-darwin".default = pkgs.buildEnv {
  #   name = "home-packages";
  #   paths = [
  #     myREnv
  #     myRStudio
  #   ];
  # };

  # devShells."aarch64-darwin".default = pkgs.mkShell {
  #   buildInputs = [
  #     myREnv
  #     myRStudio
  #   ];
  # };

  paths = [
    myREnv
    myRStudio
  ];
}
