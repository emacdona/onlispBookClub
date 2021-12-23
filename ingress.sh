#!/usr/bin/env bash
set -eo pipefail

# Use Ingress instead of:
# kubectl port-forward svc/prometheus-grafana 3000:80 &
# kubectl port-forward svc/kibana-kibana 3001:5601 &

echo 'apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: rewrite-ingress
  annotations:
    nginx.org/rewrites: "serviceName=prometheus-grafana rewrite=/;serviceName=kibana-kibana rewrite=/;serviceName=kubernetes-dashboard rewrite=/"
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
