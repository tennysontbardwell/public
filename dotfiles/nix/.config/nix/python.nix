{ lib, pkgs, ... }:
let
  python3 = pkgs.python3.override {
    self = python3;
    packageOverrides = pyfinal: pyprev: {
      snapshot-pyppeteer = pyfinal.callPackage ./snapshot-pyppeteer.nix { };
      tennyson = pyfinal.callPackage ./tennyson.py.nix { };
      matplotlib = pyprev.matplotlib.overrideAttrs (attrs: {
        enableQt = true;
      });
    };

  };

  myPythonEnv = python3.withPackages (
    ps: with ps; [
      tennyson

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

      ### Data frames & io
      numpy
      pandas
      polars
      fastparquet
      pyarrow
      # parquet TODO wait for fix

      ### ML
      jupyter
      jupyterlab
      scikit-learn
      scikit-misc
      spyder-kernels
      scipy

      ### plotting
      matplotlib
      bokeh
      tkinter
      pyqt5
      pyqt6
      opencv-python-headless
      plotnine
      altair
      vega_datasets
      pyecharts
      seaborn
      snapshot-pyppeteer
      # pyGAM
      # econML
      # parqv

      # LSP
      python-lsp-server
      yapf

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
    ]
  );

  myPythonEnvWithQt = pkgs.symlinkJoin {
    name = "python-env-with-qt";
    paths = [ myPythonEnv ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      # Wrap python to include Qt environment variables
      wrapProgram $out/bin/python \
        --prefix QT_PLUGIN_PATH : "${pkgs.qt5.qtbase.bin}/${pkgs.qt5.qtbase.qtPluginPrefix}" \
        --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath [ pkgs.qt5.qtbase ]}"
    '';
  };
in
{
  paths = with pkgs; [
    myPythonEnvWithQt
    uv
    jetbrains.pycharm-community
    pipx
  ];
}
