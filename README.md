[![Hackage](https://img.shields.io/hackage/v/disposable.svg)](https://hackage.haskell.org/package/disposable)
[![Build Status](https://secure.travis-ci.org/louispan/disposable.png?branch=master)](http://travis-ci.org/louispan/disposable)

SomeDisposable aloows storing different resource releasing actions togther in a container.
This library is useful for queueing up GHCJS.Foreign.Callback together to be released after a new rendering frame.