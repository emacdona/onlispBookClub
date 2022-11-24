all: \
	windows-wsl.eps \
	windows-wsl-docker.eps \
	windows-wsl-docker-container-A.eps \
	windows-wsl-docker-container-A-user.eps \
	windows-wsl-docker-container-A-user-windowsx.eps \
	windows-wsl-docker-container-A-user-windowsx-k3d.eps

windows-wsl.eps: diagrams.dia
		dia \
			--show-layers=Windows,WSL,WSLuser,WSLdockerCli \
			--filter=eps-pango --export=$@ $<

windows-wsl-docker.eps: diagrams.dia
		dia \
			--show-layers=Windows,WSL,WSLuser,WSLdockerCli,Docker \
			--filter=eps-pango --export=$@ $<

windows-wsl-docker-container-A.eps: diagrams.dia
		dia \
			--show-layers=Windows,WSL,WSLuser,WSLdockerCli,Docker,ContainerA,ContainerAPrograms \
			--filter=eps-pango --export=$@ $<

windows-wsl-docker-container-A-user.eps: diagrams.dia
		dia \
			--show-layers=Windows,WSL,WSLuser,WSLdockerCli,Docker,ContainerA,ContainerAPrograms,ContainerAUser \
			--filter=eps-pango --export=$@ $<

windows-wsl-docker-container-A-user-windowsx.eps: diagrams.dia
		dia \
			--show-layers=Windows,WSL,WSLuser,WSLdockerCli,Docker,ContainerA,ContainerAPrograms,ContainerAUser,WindowsX \
			--filter=eps-pango --export=$@ $<

windows-wsl-docker-container-A-user-windowsx-k3d.eps: diagrams.dia
		dia \
			--show-layers=Windows,WSL,WSLuser,WSLdockerCli,Docker,ContainerA,ContainerAPrograms,ContainerAUser,WindowsX,ContainerB,k3d \
			--filter=eps-pango --export=$@ $<

clean:
		rm -f *.eps *.pdf .*.un~
