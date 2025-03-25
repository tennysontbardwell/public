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
  ];

  myREnv = pkgs.rWrapper.override{
    packages = myRPackages;
  };

  myRStudio = pkgs.rstudioWrapper.override{
    packages = myRPackages;
  };

in
{
  paths = [
    myREnv
    myRStudio
  ];
}
