{
  lib,
  buildPythonPackage,
  fetchFromGitHub,
  setuptools,
  wheel,
  pyppeteer,
  nest-asyncio,
}:

buildPythonPackage rec {
  pname = "snapshot-pyppeteer";
  version = "0.0.2";

  src = fetchFromGitHub {
    owner = "pyecharts";
    repo = "snapshot-pyppeteer";
    rev = "c8548d598a6288ac62bf48395eac95b1ca70d49"; # v0.0.2
    sha256 = "sha256-1P/ZzadsFaHwv4OvzGwSXT3WSy0gbQ42siW+IIODFkY=";
  };

  # do not run tests
  doCheck = false;

  pythonImportsCheck = [ "snapshot_pyppeteer" ];

  propagatedBuildInputs = [
    pyppeteer
    nest-asyncio
  ];

  # specific to buildPythonPackage, see its reference
  pyproject = true;
  build-system = [
    setuptools
    wheel
  ];
}
