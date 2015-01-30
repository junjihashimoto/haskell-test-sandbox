# Test-Sandbox-Compose: Fast Development Environments Using Test-Sandbox

[![Hackage version](https://img.shields.io/hackage/v/test-sandbox-compose.svg?style=flat)](https://hackage.haskell.org/package/test-sandbox-compose)  [![Build Status](https://travis-ci.org/junjihashimoto/test-sandbox-compose.png?branch=master)](https://travis-ci.org/junjihashimoto/test-sandbox-compose) [![Coverage Status](https://coveralls.io/repos/junjihashimoto/test-sandbox-compose/badge.png)](https://coveralls.io/r/junjihashimoto/test-sandbox-compose)

Test-Sandbox-Compose makes development environments for multi-servers using Test-Sandbox.
Each server is defined in test-sandbox-compose.yml.
It is inspired by Docker Compose.

## Getting started

Install this from Hackage.

    cabal update && cabal install test-sandbox-compose


## test-sandbox-compose.yml reference

```
<service-name1>:
  cmd: <command-name>
  args:
    - <arg1>
    - <arg2>
  confs:
    - <conf1>
    - <conf2>
  dirs:
    - <dir1>
    - <dir2>
  ports:
    - <port1>
    - <port2>
<service-name2>
```


## Commands


### Up

```
test-sandbox-compose up
```

### Status

```
test-sandbox-compose status
```

### Kill

```
test-sandbox-compose kill
```

### Logs

```
test-sandbox-compose logs
```
