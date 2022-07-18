#!/usr/bin/env bash
set -eo pipefail
ISTIO_HOME="${HOME}/istio"

# https://istio.io/latest/docs/setup/getting-started/
# Override istio load balancer to NOT use ports already used by the ingress load balancer
echo 'apiVersion: install.istio.io/v1alpha1
kind: IstioOperator
spec:
  components:
    ingressGateways:
    - name: istio-ingressgateway
      enabled: true
      k8s:
        service:
          ports:
            - port: 15021
              targetPort: 15021
              name: status-port
            - port: 8080
              targetPort: 8080
              name: http2
            - port: 8443
              targetPort: 8443
              name: https
            - port: 31400
              targetPort: 31400
              name: tcp
            - port: 15443
              targetPort: 15443
              name: tls
' | istioctl install --set profile=demo -y -f -
# To look at the resulting manifests
# istioctl manifest generate --dry-run --set profile=demo -f -

kubectl label namespace default istio-injection=enabled

# https://istio.io/latest/docs/setup/getting-started/
kubectl apply -f ${ISTIO_HOME}/samples/addons

kubectl apply -f - <<EOF
apiVersion: networking.istio.io/v1alpha3
kind: Gateway
metadata:
  name: helloworldlisp-gateway
spec:
  selector:
    istio: ingressgateway
  servers:
  - port:
      number: 8080
      name: http
      protocol: HTTP
    hosts:
    - "helloworld.lisp.test"
EOF

kubectl apply -f - <<EOF
apiVersion: networking.istio.io/v1alpha3
kind: VirtualService
metadata:
  name: helloworldlisp
spec:
  hosts:
  - "helloworld.lisp.test"
  gateways:
  - helloworldlisp-gateway
  http:
  - route:
    - destination:
        host: hw-helloworld
        port:
          number: 8282
EOF

echo "Run this:"
echo "istioctl dashboard kiali"

