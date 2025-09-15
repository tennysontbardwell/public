{ lib, pkgs, ... }:
let
  myPythonEnv = pkgs.python3.withPackages (ps: with ps;
    [
      ### misc/basic
      requests
      lxml
      beautifulsoup4
      pypdf
      tqdm
      pip
      aiohttp
      scrapy
      pipe

      ### Data
      matplotlib
      numpy
      pandas
      polars
      fastparquet
      # parquet TODO wait for fix
      jupyter
      jupyterlab
      plotnine
      scikit-learn
      scikit-misc
      spyder-kernels
      scipy
      # pyGAM
      # econML
      # parqv

      ### AI
      openai
      torch
      transformers
      torchvision
      torchaudio
      huggingface-hub
      # vllm

      ### Misc
      yfinance
      jsonpickle
      simple-term-menu

      ## not FOSS / not mac
      # accelerate
      # bitsandbytes
  ]);
in
{
  paths = with pkgs; [
    myPythonEnv
    uv
    jetbrains.pycharm-community
    pipx
  ];
}
