apiVersion: certificates.k8s.io/v1
kind: CertificateSigningRequest
metadata:
   name: cluster-admin
spec:
   request: __CSR__
   signerName: kubernetes.io/kube-apiserver-client
   expirationSeconds: 86400  # one day
   usages:
      - client auth
