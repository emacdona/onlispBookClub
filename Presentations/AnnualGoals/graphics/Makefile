all: \
	010.eps \
	020.eps \
	030.eps \
	040.eps \
	050.eps \
	060.eps \
	070.eps \
	080.eps \
	090.eps \
	100.eps \
	110.eps \
	120.eps \
	130.eps

clean:
		rm -f *.eps *.pdf .*.un~

010.eps: diagrams.dia
		dia \
			--show-layers=\
Windows \
			--filter=eps-pango --export=$@ $<

020.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX \
			--filter=eps-pango --export=$@ $<

030.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL \
			--filter=eps-pango --export=$@ $<

040.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser \
			--filter=eps-pango --export=$@ $<

050.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli \
			--filter=eps-pango --export=$@ $<

060.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker \
			--filter=eps-pango --export=$@ $<

070.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker,\
ContainerA \
			--filter=eps-pango --export=$@ $<

080.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker,\
ContainerA,\
ContainerAPrograms \
			--filter=eps-pango --export=$@ $<

090.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker,\
ContainerA,\
ContainerAUser,\
ContainerAPrograms \
			--filter=eps-pango --export=$@ $<

100.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker,\
ContainerA,\
ContainerAUser,\
ContainerAPrograms,\
ContainerAUserMapping \
			--filter=eps-pango --export=$@ $<

110.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker,\
ContainerA,\
ContainerAUser,\
ContainerAPrograms,\
ContainerAUserMapping,\
ContainerACliMapping \
			--filter=eps-pango --export=$@ $<

120.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker,\
ContainerA,\
ContainerAUser,\
ContainerAPrograms,\
ContainerAUserMapping,\
ContainerACliMapping,\
ContainerB \
			--filter=eps-pango --export=$@ $<

130.eps: diagrams.dia
		dia \
			--show-layers=\
Windows,\
WindowsX,\
WSL,\
WSLuser,\
WSLdockerCli,\
Docker,\
ContainerA,\
ContainerAUser,\
ContainerAPrograms,\
ContainerAUserMapping,\
ContainerACliMapping,\
ContainerB,\
k3d \
			--filter=eps-pango --export=$@ $<

