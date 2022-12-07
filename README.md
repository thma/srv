# srv
srv is a tiny web server for local deployments

## a self signed certificate for testing the https server:

```bash
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
```
