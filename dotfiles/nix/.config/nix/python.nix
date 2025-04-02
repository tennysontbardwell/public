{
  lib,
  pkgs,
  pyproject-nix,
  uv2nix,
  pyproject-build-systems,
  ...
}:
let
  myPythonEnv = pkgs.python3.withPackages (ps: with ps; with pkgs.python312Packages;
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
      parquet
      jupyter
      jupyterlab
      plotnine
      scikit-learn
      scikit-misc
      spyder-kernels
      scipy
      # pyGAM
      # econML

      ### AI
      openai
      parquet
      torch
      transformers
      torchvision
      torchaudio
      huggingface-hub
      # vllm

      ## misc from vllm debugging
      aioprometheus
      fastapi
      lm-format-enforcer
      outlines
      psutil
      py-cpuinfo
      pyarrow
      pydantic
      pyzmq
      ray
      sentencepiece
      tiktoken
      uvicorn
      xformers
      # prometheus-fastapi-instrumentator

      ## not FOSS / not mac
      # accelerate
      # bitsandbytes
  ]);

  workspace = uv2nix.lib.workspace.loadWorkspace { workspaceRoot = ./.; };

  python = pkgs.python312;
  pyprojectOverrides = _final: _prev: {
    # Implement build fixups here.
    # Note that uv2nix is _not_ using Nixpkgs buildPythonPackage.
    # It's using https://pyproject-nix.github.io/pyproject.nix/build.html
  };
  pythonSet =
    # Use base package set from pyproject.nix builders
    (pkgs.callPackage pyproject-nix.build.packages {
      inherit python;
    }).overrideScope
      (
        lib.composeManyExtensions [
          pyproject-build-systems.overlays.default
          # overlay
          pyprojectOverrides
        ]
      );

in
{
  paths = with pkgs; [
    myPythonEnv
    uv
    jetbrains.pycharm-community
    pipx
  ];

  venv = builtins.attrValues (pythonSet.mkVirtualEnv "hello-world-env" workspace.deps.default);
}
