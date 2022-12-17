# srv
srv is a tiny web server for local deployments

## Installation

## build from source

```bash

$ git clone https://github.com/thma/srv.git
$ cd srv
$ stack install 

-- or if you don't have stack installed

$ cabal install
```

## Usage

```bash
$ srv

starting up srv...
srv.yaml not found, generating file with default config
Starting HTTP server on port 8080
Starting HTTPS server on port 8443
```

During the initial run, srv will create a config file "srv.yaml" 
in the current directory. 

You can edit this file to change the port, the directory to serve, 
and also select whether host HTTP or HTTPS or both.

If HTTPS is selected, you can also provide a certificate and key file.
If you don't provide both files, srv will use a predefined
self-signed certificate which is meant for local demo use only.
(using this demo certificate will result in a warning in your browser 
and some browsers will even refuse to connect.)

## Configuration

Here is a sample configuration file:

```yaml
documentRoot: .
handlers:
- - HTTP
  - 8080
- - HTTPS
  - 8443
pathToCert: certificate.pem
pathToKey: key.pem
```

## How to generate your own certificate

```bash
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
```




