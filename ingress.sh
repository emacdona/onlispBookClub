#!/usr/bin/env bash
set -eo pipefail

# Use Ingress instead of:
# kubectl port-forward svc/prometheus-grafana 3000:80 &
# kubectl port-forward svc/kibana-kibana 3001:5601 &

# If using multiple Ingress-es with the same host, you need to use mergeable
# ingress types.
# https://github.com/nginxinc/kubernetes-ingress/issues/76
# https://github.com/nginxinc/kubernetes-ingress/tree/master/examples/mergeable-ingress-types

echo 'apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: master-ingress
  annotations:
    nginx.org/mergeable-ingress-type: "master"
spec:
  ingressClassName: nginx
  rules:
  - host: host.docker.internal
' | kubectl create -f -

echo 'apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: infrastructure-ingress
  annotations:
    nginx.org/rewrites: "serviceName=prometheus-grafana rewrite=/;serviceName=kibana-kibana rewrite=/;serviceName=kubernetes-dashboard rewrite=/"
    nginx.org/mergeable-ingress-type: "minion"
spec:
  ingressClassName: nginx
  rules:
  - host: host.docker.internal
    http:
      paths:
      - path: /grafana/
        pathType: Prefix
        backend:
          service:
            name: prometheus-grafana
            port:
              number: 80
      - path: /kibana/
        pathType: Prefix
        backend:
          service:
            name: kibana-kibana
            port:
              number: 5601
' | kubectl create -f -
