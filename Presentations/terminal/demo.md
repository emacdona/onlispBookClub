%title: Running Programs with Docker on your Linux Workstation 
%author: Ed MacDonald - Solution Street
%date: 2022-02-10

-> Running Programs with Docker on your Linux Workstation <-
=========

-------------------------------------------------
-> Who's heard of [Octave](https://www.gnu.org/software/octave/)? <-

<br>
-> Hopefully no one. It's more fun that way. <-

-------------------------------------------------

-> # Goals (What do we want?) <-

<br>
* Use some software ( *Octave* ) without leaving an *install* footprint on our host machine
<br>
* Use that software to create/modify files on the host
<br>
* End up being the owner of files we create on the host!
<br>
* GUI
<br>
   - Ed, are you serious?  
      <br> 
   - *We'll see...*

-------------------------------------------------

-> # The Dockerfile <-

```
| FROM ubuntu:20.04
| 
| RUN apt-get update && \ 
|     DEBIAN_FRONTEND=noninteractive \ 
|     apt-get install -y octave
|
| ARG UID
| ARG GID
| ARG USERNAME
| ARG USERGROUP
|
| RUN groupadd -g "${GID}" "${USERGROUP}"
| 
| RUN useradd \ 
|     -d "/home/${USERNAME}" \ 
|     -ms /bin/zsh \ 
|     -g "${USERGROUP}" \ 
|     -u $UID "${USERNAME}"
| 
| RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> \ 
|     /etc/sudoers
| 
| USER "${USERNAME}"
| 
| WORKDIR "/home/${USERNAME}/workdir"
| 
| COPY myPlot.m myPlot.m
| 
| CMD ["octave", "--no-gui", "--quiet"]
```

-------------------------------------------------

-> # Let's Kick the Tires <-

-> (translation: "Demo Time") <-

-------------------------------------------------

-> # Thanks for watching! <-


## ... and thanks to the following tools
* [mdp](https://github.com/visit1985/mdp)
* [demo-magic](https://github.com/paxtonhare/demo-magic)
* [octave](https://www.gnu.org/software/octave/)
* [docker](https://www.docker.com/)

-------------------------------------------------

-> # We've still got time left?! <-

<br>
-> ## Let's keep going... <-

<br>
* Why Linux and not Mac?
<br>
   * Docker on Mac is virtualized.
<br>
   * Bind mounts to the "host" (OS X) filesystem have to traverse a VM (Linux) layer.
<br>
   * No idea how this happens (NFS?), but...
<br>
   * This makes bind mounts VERY slow.
<br>
* Okay, why Linux and not Windoze + WSL?
<br>
   * Well, I'm not sure. I just haven't tried it.
<br>
   * I *think* WSL is native (ie: NOT emulated Linux).
<br>
   * This *could* mean that bind mounts will work really well on Windoze!
<br>
* X Windows
<br>
   * For either OS X or Windoze, you'll need to install an X Server if you want a GUI.
<br>
   * You'll probably have to connect to it over TCP/IP instead of Unix Domain Sockets.
