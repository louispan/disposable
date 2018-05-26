[![Hackage](https://img.shields.io/hackage/v/disposable.svg)](https://hackage.haskell.org/package/disposable)
[![Build Status](https://secure.travis-ci.org/louispan/disposable.png?branch=master)](http://travis-ci.org/louispan/disposable)

Disposable allows storing different resource releasing actions togther in a container.
This library is useful for queueing up GHCJS.Foreign.Callback together to be released after a new rendering frame.

# Changelog

* 1.0.0.0
  - Breaking changes:
    - Simplified by removing SomeDisposable GADT; Disposable is now a newtype, and the typeclass is called Dispose.
    - The intention is no longer to create Disposable instances for everything.
    - Disposable is only used to provide a safe wrapper around IO to ensure that it performs no other side effects.