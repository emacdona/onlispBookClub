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
spec:
  ingressClassName: nginx
  rules:
  - host: grafana.test
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: prometheus-grafana
            port:
              number: 80
  - host: kibana.test
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: kibana-kibana
            port:
              number: 5601
' | kubectl create -f -

