# Z80 Single Board Computer with CP/M
A simple SBC that uses Compact Flash for program storage and runs CP/M based on [Grant Searle's Z80 Computer](http://searle.x10host.com/cpm/index.html). The primary differences between Grant's design and this computer are the use of a GAL22V10 as 'glue' logic to reduce the chip count and the addition of an [8255 PPI](https://en.wikipedia.org/wiki/Intel_8255) for parallel I/O. I've also made a couple of tweaks to 'monitor.asm'. Assemble the source files with the [Macro Assembler AS](http://john.ccac.rwth-aachen.de:8000/as/)

<p align="center"><img src="/images/Z80 SBC CPU.png"/>
<p align="center">Z80 SBC CPU</p><br>

<p align="center"><img src="/images/Z80 SBC GAL22V10.png"/>
<p align="center">Z80 SBC GAL20V10</p><br>

<p align="center"><img src="/images/Z80 SBC CF.png"/>
<p align="center">Z80 SBC Compact Flash</p><br>

<p align="center"><img src="/images/Z80 SBC Serial.png"/>
<p align="center">Z80 SBC Serial I/O</p><br>

<p align="center"><img src="/images/Z80 SBC PPI.png"/>
<p align="center">Z80 SBC Parallel I/O</p><br>

<p align="center"><img src="/images/Z80 SBC Reset.png"/>
<p align="center">Z80 SBC Reset</p><br>
