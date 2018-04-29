# Ereina

Natural Language Processing (NLP) for Persian language as a service.

## Features:

* Written in Haskell
* RESTful API (Integration is seamless)

## Installaion 

There are some methods for installing Ereina on your system.

* First method, we recommend Haskell build tool [stack](https://haskell-lang.org/get-started):

```sh
$ stack build
$ stack exec Ereina
```

**Note**: For testing before installation run:

```sh
$ stack test
```

* Second method, as a docker container (a RESTful server behind nginx):

To be written ...

## Usage and Methods

* version: this method returns a message regarding Ereina's version

```sh
curl -GET  http://localhost:2319/version 

$ {"version":"0.1.0.0","message":"Hi there, I am Ereina."}% 
```

* fixSpaces: given a document removes the redundant spaces from the document

```sh
curl -d '{"document":" باید چاره ای برای   این کار اندیشید    " }' -H "Content-Type: application/json" -X POST http://localhost:2319/fixSpaces

$ {"document":"باید چاره ای برای این کار اندیشید"}% 
```

## Todos

* Adding tests
* Adding more methods
* Writing more doumentation

