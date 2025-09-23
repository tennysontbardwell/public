{
  lib,
  buildPythonPackage,
  fetchPypi,
  setuptools,
  wheel,
  pyppeteer,
}:

buildPythonPackage rec {
  pname = "snapshot-pyppeteer";
  version = "0.0.2";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-CP3V73yWSArRHBLUct4hrNMjWZlvaaUlkpm1QP66RWA=";
  };

  # do not run tests
  doCheck = false;

  propagatedBuildInputs = [
    pyppeteer
  ];

  # specific to buildPythonPackage, see its reference
  pyproject = true;
  build-system = [
    setuptools
    wheel
  ];
}
