---
title       : Adhesion and friction in 1D and 2D systems 
subtitle    : 
author      : Colin Bousige
job         : Post-Doctoral Researcher, Institut Lumière Matière, Lyon, France
framework   : revealjs        # {io2012, html5slides, shower, dzslides, ...}
logo        : NT_eau.png
adress      : "http://ilm-perso.univ-lyon1.fr/~cbousige/files/Seminaire/Friction/"
revealjs:
  theme: perso
  transition: none
  loop: true
  font-family: "Computer Modern"
  center: "true"
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [mathjax]     # {mathjax, quiz, bootstrap}
mathjax:
    preferredFont: "SansSerif"
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
output      : pdf
--- bg:white

<div class="hidden">
   <script type="text/javascript">
      <!--//--><![CDATA[//><!--
         var images = new Array()
         function preload() {
            for (i = 0; i < preload.arguments.length; i++) {
               images[i] = new Image()
               images[i].src = preload.arguments[i]
            }
         }
         preload(
            "assets/img/remove.gif"
            "assets/img/CSxx.gif"
         )
      //--><!]]>
   </script>

   <script type="text/x-mathjax-config">
   MathJax.Hub.Config({
     "HTML-CSS": {availableFonts: ["TeX"]},
          MMLorHTML: {prefer: "HTML"}
   });
   MathJax.Hub.Register.StartupHook("HTML-CSS Jax Ready",function () {
     var VARIANT = MathJax.OutputJax["HTML-CSS"].FONTDATA.VARIANT;
     VARIANT["normal"].fonts.unshift("MathJax_SansSerif");
     VARIANT["bold"].fonts.unshift("MathJax_SansSerif-bold");
     VARIANT["italic"].fonts.unshift("MathJax_SansSerif-italic");
     VARIANT["-tex-mathit"].fonts.unshift("MathJax_SansSerif-italic");
   });
   MathJax.Hub.Register.StartupHook("SVG Jax Ready",function () {
     var VARIANT = MathJax.OutputJax.SVG.FONTDATA.VARIANT;
     VARIANT["normal"].fonts.unshift("MathJax_SansSerif");
     VARIANT["bold"].fonts.unshift("MathJax_SansSerif-bold");
     VARIANT["italic"].fonts.unshift("MathJax_SansSerif-italic");
     VARIANT["-tex-mathit"].fonts.unshift("MathJax_SansSerif-italic");
   });
   </script>
</div>


<!-- 
Run with :
setwd("/Users/colin/Travail/Conferences/2016/Seminaire_Bordeaux");Sys.setlocale("LC_ALL", 'en_US.UTF-8');library(slidify);
slidify("index.Rmd") 
-->

<center><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><span style='color:black;font-size:20px;text-align:center'>
Swipe or press right to start the presentation - Pinch out or press ESC to see all the slides<br>
Swipe or press down to see additional slides when available (see navigation arrows on the right)<br>
Press F to set fullscreen - Press Alt and click to zoom in or out - Press B to fade to black - Press S for speaker view<br>
Slides made using <a href="http://slidify.org/">Slidify</a> with 
<a href="http://lab.hakim.se/reveal-js/?transition=fade#/">Reveal.js</a> framework</span></center>

--- dt:fade &titleslide


--- dt:fade 

## Main thematics of my research
### PhD Thesis: 1D physics in carbon peapods


<img src="assets/img/peapods_TEM2.png" style='margin-top:3%;width:30%;'/>
<img src="assets/img/peapods_diagramme.png" style='margin-top:2%;width:80%;'/>


<div style='width:100%;text-align:left;margin-top:1%;font-size:18px'>
<center>
<a href="http://prb.aps.org/abstract/PRB/v87/i19/e195438">Bousige et al., <i>Phys. Rev. B</i> <b>87</b> (2013) 195438</a>
$\bullet$ <a href="http://prb.aps.org/abstract/PRB/v86/i4/e045446">Bousige et al., <i>Phys. Rev. B</i> <b>86</b> (2012) 045446</a><br>
<a href="http://prb.aps.org/abstract/PRB/v87/i19/e195438">Rols et al., <i>Eur. Phys. J.: Special Topic</i> <b>213</b> (2013) 77</a>
$\bullet$ <a href="http://iopscience.iop.org/0295-5075/103/6/66002/">Bousige et al., <i>Eur. Phys. Lett.</i> <b>103</b> (2013) 66002</a>
</center>
</div>


---  &threecol w1:30% w2:68% w3:30%

## Main thematics of my research
### Post-Doc @ MIT: reconstructing molecular models of kerogen

***=left
<img src="assets/img/ker_TEM.png" style='margin-top:30%;width:80%;'/>

***=middle
<img src="assets/img/ker_dos_psd_simul.png" style='margin-top:7%;width:100%;'/>

***=right
<!-- <img src="assets/img/CSxx.gif" style='margin-top:20%;width:100%;'/> -->

***=full

<div style='width:70%;text-align:left;margin-top:3.5%;font-size:18px'>
<center>
<a href="http://scitation.aip.org/content/aip/journal/jcp/142/11/10.1063/1.4914921">Bousige, Botan, Ulm, Pellenq, Coasne, <i>J. Chem. Phys.</i> <b>142</b> (2015) 114112</a><br>
<a href="http://www.nature.com/nmat/journal/vaop/ncurrent/full/nmat4541.html"> Bousige et al., <i>Nature Materials</i> <b>15</b> (2016) 576</a>
</center>
</div>


---  &threecol w1:30% w2:40% w3:30%

## Main thematics of my research
### Post-Doc @ MIT: reconstructing molecular models of kerogen

***=left
<img src="assets/img/ker_TEM.png" style='margin-top:30%;width:80%;'/>

***=middle
<img src="assets/img/ker_dos_psd_simul2.png" style='margin-top:11%;margin-left:4%;width:100%;'/>

***=right
<img src="assets/img/CSxx.gif" style='margin-top:20%;width:100%;'/>
<center>
<div style='width:100%;margin-top:1%;font-size:18px'>Tortuosity and adsorption in <br>amorphous porous materials<br>with P. Levitz and B. Coasne<br>(in preparation)</div>
</center>

***=full

<div style='width:70%;text-align:left;margin-top:-5%;font-size:18px'>
<center>
<a href="http://scitation.aip.org/content/aip/journal/jcp/142/11/10.1063/1.4914921">Bousige, Botan, Ulm, Pellenq, Coasne, <i>J. Chem. Phys.</i> <b>142</b> (2015) 114112</a><br>
<a href="http://www.nature.com/nmat/journal/vaop/ncurrent/full/nmat4541.html"> Bousige et al., <i>Nature Materials</i> <b>15</b> (2016) 576</a>
</center>
</div>

---  &threecol w1:30% w2:45% w3:25%

## Main thematics of my research
### Post-Doc @ ILM: 1D and 2D nano-objects under pressure

***=left
<img src="assets/img/Graphene.jpg" style='margin-top:35%;width:100%;'/>

***=middle
<img src="assets/img/graph_DAC.png" style='margin-top:25%;width:100%;'/>

***=right
<img src="assets/img/NT_eau.png" style='margin-top:35%;width:100%;'/>

***=full
<div style='width:100%;text-align:left;margin-top:1%;font-size:18px'>
<center>
<br>Bousige et al., <i>Nano Lett.</i> (in reviewing process)</a>
</center>
</div>


--- dt:fade &twocol w1:45%;margin-left:10% w2:35%;margin-right:10%

## Main thematics of my research
### Summary
<br><br><br>

***=left

- __Structure and dynamics of carbon nanostructures__
- __Confinement and phase transitions__
- __Mechanical properties__
- __Transport in complex media__
   - __Adhesion and friction__


***=right

- Experimental
   - Inelastic Neutron Scattering
   - Diffraction (x-rays, neutrons)
   - Raman spectroscopy
   - High pressures
- Simulations
   - Molecular Dynamics
   - Monte Carlo


--- bg:black  dt:fade 

<span style="color:white"> Menu </span>
---------------------------------------------------------------------
<br><br>
<center>
<span style='color:white;font-size:25pt'> 
I - Biaxial strain transfer in supported graphene
<img src="assets/img/Graphene.jpg" style='margin-top:2%;width:20%;'/><br>
II - Pinned and unpinned states in carbon peapods:<br>insights into nanotube super-lubricity
<img src="assets/img/peapods_TEM2.png" style='margin-top:2%;width:50%;'/>
</span>
</center>


--- bg:grey dt:fade

<span style="color:white;text-shadow: 10px 10px 10px rgb(1, 1, 1);"> Chapter I </span>
========================
<span style="color:white"> Biaxial strain transfer in supported graphene </span>
--------------------------------------------------------------------------------

<br>
<center style="color:white;font-size:20px">Post-Doc<br>Institut Lumière Matière (Lyon)</center>
<img src="assets/img/Graphene.jpg" style='margin-top:5%;width:30%;border: 4px solid #333333;box-shadow: 5px 5px 5px rgb(1, 1, 1);'/>



--- dt:fade &twocol w1:55% w2:45% 

## Exploring strain transfer in supported graphene

***=right

<img src="assets/img/graph_exemple.png" style='margin-top:10%;'/>
<center>From Ni et al. _ACS Nano_ __2__ (2008) 2301</center>

***=left
<br>
<br>

__Context of strain-tronics with graphene:__

Optical and electronic properties can be tuned by the application of strain

$\Rightarrow$ Applications in electronics, sensors, etc.

In the aim of using graphene (or TMDs) in such devices, one need a reproducible strain production.

Graphene is most of the time deposited on a substrate.

***=full

<br>

> How efficiently is strain transferred? 

> What are the limits of strain transfer?


--- &twocol w1:40%;margin-left:5% w2:55% 

## Exploring strain transfer in supported graphene
### Experimental approach

***=left
<br><br>
- <p class="fragment highlight-current-red" data-fragment-index="1">Evolution of the Raman G-band </p>
- <p class="fragment highlight-current-red" data-fragment-index="2">Biaxial strain in a Diamond Anvil Cell</p>
- <p class="fragment highlight-current-red" data-fragment-index="3">Various substrates (Diamond, Sapphire, SiO$_2$)</p>
<br>

<ul style='color:rgba(0,0,0,.3)'> 
   <li class="fragment highlight-current-blue" data-fragment-index="1">
   Highly sensitive to in-plane strain</li>
   <li class="fragment highlight-current-blue" data-fragment-index="2">
   Compression due to volume reduction of the substrate </li>
   <li class="fragment highlight-current-blue" data-fragment-index="2">
   Maximization of the conformation of graphene to its substrate by the application of high pressures in a DAC with alcohol pressure transmitting medium (PTM)</li>
   <li class="fragment highlight-current-blue" data-fragment-index="3">
   Various bulk moduli</li>
</ul>


***=right
<img src="assets/img/gband.png" style='margin-top:5%;width:60%;'/>
<img src="assets/img/graph_DAC.png" style='margin-top:1%;width:100%;'/>


--- &twocol w1:40% w2:60% 

## Exploring strain transfer in supported graphene
### Results

***=right
<img src="assets/img/graph_evol.png" style='margin-top:10%;width:100%;'/>

***=left

<br><br>

|            | $\partial\omega_G/\partial P$ | <span style="margin-left:20px" class="fragment fade-in" data-fragment-index="1" >$\beta_L$</span> |
|:----------:|:-----------------------------:|:-----------------------------------------------:|
|            |  <b>(cm$^{-1}$/GPa)</b>        | <b><span style="margin-left:20px" class="fragment fade-in" data-fragment-index="1" > (GPa)   </span></b> |
|<b>Diamond </b>|         $ 4.0\pm0.2$          | <span style="margin-left:20px" class="fragment fade-in" data-fragment-index="1" > $1329$  </span> |
|<b>Graphite</b>|         $ 4.4\pm0.3$          | <span style="margin-left:20px" class="fragment fade-in" data-fragment-index="1" > $1250$  </span> |
|<b>Sapphire</b>|         $ 7.8\pm0.5$          | <span style="margin-left:20px" class="fragment fade-in" data-fragment-index="1" > $ 750$  </span> |
|<b>Copper  </b>|         $ 9.2\pm0.3$          | <span style="margin-left:20px" class="fragment fade-in" data-fragment-index="1" > $ 420$  </span> |
|<b>SiO$_2$ </b>|         $10.5\pm0.2$          | <span style="margin-left:20px" class="fragment fade-in" data-fragment-index="1" > $ 114$  </span> |

<br>
<span style="margin-top:5%" class="fragment fade-in" data-fragment-index="1" >
<center>Different substrates : different slopes<br>
   __How do we explain that?__</span></center>
<br>
<blockquote class="fragment fade-in" data-fragment-index="1" >
   $\Rightarrow$ Pressure is not the good variable
</blockquote>


--- 

## Exploring strain transfer in supported graphene
### Renormalization to the relevant physical quantities $\varepsilon$ and $\sigma$

<br>
<div style='float:left;width:45%;margin-left:5%;line-height:1.5em'>
<br>
<ol>
   <li> For 2D materials, <i>Pressure</i> of the PTM and <i>Stress</i> of the material are <b>not equivalent</b></li>
   <li> Graphite is a 3D material, so $P\Leftrightarrow\sigma$</li>
   <li> For a Raman shift of the deposited graphene corresponds a stress of the graphene flake:$$\sigma_G^S=\Delta\omega_G^S/\Omega_{gr}$$</li>
   <li> The strain ε of the graphene flake and the strain of the substrate can be retrieved through the Birch-Murnaghan equation of states:
   $$\frac{r}{r_0}=\left(1+\sigma\frac{\beta'}{\beta}\right)^{-1/\beta'}$$</li>
</ol>

</div>
<div style='float:right;width:50%'>
   <img src="assets/img/graph_renorm.png" style='margin-top:5%;width:55%;'/>
</div>

--- 

## Exploring strain transfer in supported graphene
### After renormalization to the relevant physical quantities $\varepsilon$ and $\sigma$

<div style='float:left;width:65%;'>
<img src="assets/img/graph_epsilon.png" style='margin-top:1%;width:100%;'/>
</div>
<div style='float:right;width:35%;margin-top:10%'>
<blockquote style='background:rgba(76,179,246,0.2);color:black;'> 
$\varepsilon_G-\varepsilon_S\neq$0 <br> $\Leftrightarrow$ <br> Partial strain transmission
</blockquote>
<br><br><br><br><br>
<blockquote> $\varepsilon_G-\varepsilon_S=$0 <br> $\Leftrightarrow$ <br> Total strain transmission</blockquote>
</div>



--- 

## Exploring strain transfer in supported graphene
### Quantifying the efficiency of strain transfer

<div style='float:left;width:35%;margin-top:5%'>
$\varepsilon_G=\alpha\varepsilon_S$ <br><br>
$\varepsilon_S = P/\beta_S$<br><br>
$\varepsilon_G = \sigma_G/\beta_{ref}$<br><br>
<blockquote style='background:rgba(76,179,246,0.2);color:black;'>$\sigma_G = \alpha(\sigma_G)\frac{\beta_{ref}}{\beta_S}P$</blockquote><br><br>
with ref=graphite:<br> $\Omega_{ref}=4.4$ cm$^{-1}$/GPa <br> $\beta_{ref}=1250$ GPa
</div>
<div style='float:right;width:55%;margin-right:5%'>
<img src="assets/img/graph_alpha.png" style='margin-top:6%;width:100%;'/>
</div>

--- &vertical

## Exploring strain transfer in supported graphene
### Quantifying the efficiency of strain transfer

<div style='float:left;width:65%;'>
<img src="assets/img/alpha_sigma.png" style='float:left;margin-top:5%;width:100%;'/>
</div>
<div style='float:right;width:35%;margin-top:8%'>
<blockquote> Total strain transmission</blockquote>
<br><br><br>
<blockquote style='background:rgba(76,179,246,0.4);color:black;'>Partial strain transmission</blockquote>
Creation of a stress field
</div>


*** 

### Where does the energy go? 

- ~~Defects/tearing~~
- Buckling/slipping/stress field

<img src="assets/img/graph_sio2_avant_apres.png" style='float:left;margin-top:5%;width:40%;'/>
<img src="assets/img/graph_evol.png" style='float:right;margin-top:3%;width:50%;'/>


--- 

## Exploring strain transfer in supported graphene
### Developing a tool to check for strain transfer efficiency

<br>
<div style='float:left;width:55%;'>
$\frac{\partial\Delta\omega_G}{\partial\varepsilon} = -57\pm2~\text{cm}^{-1}/\%$
<img src="assets/img/graph_compa.png" style='margin-top:5%;width:100%;'/>
</div>
<div style='float:right;width:44%;margin-right:1%;margin-top:10%;line-height:2em'>
<ul>
   <li><a href="http://pubs.acs.org/doi/abs/10.1021/nl101533x">Ding et al., Nano Lett. (2010)</a>:<br>Piezo-actuators, sandwiched graphene</li>
   <li><a href="http://www.nature.com/articles/srep18219">Androulidakis et al., Sci. Reports (2015)</a>:<br>Deposited graphene, bending substrate</li>
   <li><a href="http://scitation.aip.org/content/aip/journal/apl/108/22/10.1063/1.4952972">Shin et al., Appl. Phys. Lett. (2016)</a>:<br>Glass blower method (suspended graphene)</li>
</ul>
</div>


--- dt:fade

## Supported Graphene - Conclusions

<br>
<br>
<br>
<center>
<div style='line-height:2em;width:80%;'>
<ul>
   <li class="fragment highlight-current-blue" data-fragment-index="1"> Total strain transfer (adhesion) should never be assumed, but carefully checked</li>
   <li class="fragment highlight-current-blue" data-fragment-index="2"> How to check this? Easy tool:</li>
   <ul class="fragment highlight-current-blue" data-fragment-index="2">
      <li>Total strain transmission: $\frac{\partial\Delta\omega_G}{\partial\varepsilon} = -57\pm2~\text{cm}^{-1}/\%$ </li>   
      <li>Partial strain transmission:$\frac{\partial\Delta\omega_G}{\partial\varepsilon} > -57\pm2~\text{cm}^{-1}/\%$ </li>   
   </ul>
   <li class="fragment highlight-current-blue" data-fragment-index="3">Still need to explore:</li>
   <ul class="fragment highlight-current-blue" data-fragment-index="3">
      <li> The possible correlation between $\sigma_G^c$ and the percolation of the locally stressed regions</li>
      <li> The role of the substrate roughness and the graphene microstructure</li>
</ul>
</div>
</center>


--- &vertical 

## Work in progress: _suspended_ objects
### Goals:

- Studying various individual nano-objects at high pressure with Raman or SMS spectroscopies, _e.g_:
  - sp$^2$-sp$^3$ transition in bi-layer graphene at high pressures ($\gtrsim$10 GPa)
  - optical absorption of individual nanotube under pressure (SMS)
  - you name it...

<img src="assets/img/cut_001.png" style='margin-top:2%;width:50%;'/>


*** 

## Transferring the substrate

<img src="assets/img/remove.gif" style='margin-top:10%;width:40%;'/>
<img src="assets/img/remove.gif" style='margin-top:100%;width:0%;'/>
<!-- don't know why but have to add a second gif so that the first loops... -->

*** 

## Spatial Modulation Spectroscopy (SMS)

<img src="assets/img/SMS.png" style='margin-top:1%;width:85%;'/>



--- bg:grey dt:fade

<span style="color:white;text-shadow: 10px 10px 10px rgb(1, 1, 1);"> Chapter II </span>
========================
<span style="color:white"> Pinned and unpinned states in carbon peapods:<br>insights into nanotube super-lubricity </span>
---------------------------------------------------------------------
<br>
<center style="color:white;font-size:20px">PhD Thesis<br>Institut Laue Langevin (Grenoble) and Laboratoire de Physique des Solides (Orsay)</center>

<img src="assets/img/peapods_TEM2.png" style='margin-top:10%;width:60%;border:4px solid #333333;box-shadow: 5px 5px 5px rgb(1, 1, 1);'/>


--- &twocol w1:48% w2:48% 

## Carbon nano-peapods: a model 1D system

<div style='float:left;width:40%;margin-left:5%'>
<center><b>Two different samples:</b></center>
<img src="assets/img/peapods_TEM.png" style='width:60%;'/>
<p stye='align:left'>
Monomers:
<img src="assets/img/rotations.png" style='width:80%;'/>
Polymers: 
<img src="assets/img/libration.png" style='width:80%;'/>
</p>
</div>

<div style='float:right;width:40%;margin-right:5%' class="fragment fade-in">
<center><b>Observing their dynamics with INS:</b></center>
<img align:"middle" src="assets/img/raw_data_helvetica.png" style='width:450px;margin-top:10px'/>
</div>


--- 

## Carbon nano-peapods: a model 1D system

<img src="assets/img/L_T_peapods.png" style='width:800px;'/>


--- &vertical 

## Carbon nano-peapods: a model 1D system
<div style='float:left;width:45%'>
<img src="assets/img/L_T_peapods.png" style='margin-top:10%'/>
<img src="assets/img/rotations.png"/>
</div>

<div style='float:right;width:55%'>
   <br>
   <h3>Mid T: Finite size 1D crystal</h3>
   <ul> 
      <li> Elastic diffusion $\rightarrow$ 1D crystal </li>
      <li> Quasi-elastic diffusion in Mono and Polymers $\rightarrow$ measure speed of sound along C$_{60}$ chains (3.5 km/s)</li>
   </ul>
   <p style='text-align:left'>
   <a href="http://prb.aps.org/abstract/PRB/v87/i19/e195438" style='font-style: italic'>Bousige et al., PRB <b>87</b> (2013) 195438</a><br><br><br>
   Elastic nature:</p> <br>
   <img align:"top" src="assets/img/nature_el.png" style='float:left;height:300px;'/><img src="assets/img/peapods_kis_HT2.png" style="width:60%;" />
</div>

***

<center>__Elastic nature from inelastic data:__</center>

<div style='float:left;width:40%;margin-left:5%'>
<img src="assets/img/peapods_translations_INS.png" style='margin-top:30%'/>
</div>
<div style='float:right;width:40%;margin-right:5%;margin-top:2%'>
<img src="assets/img/peapods_kis_HT.png"/>
</div>


***

<center>__Simulation of $S(Q,\omega)$ for various geometries__</center>

<center>
<div style='width:30%;'>
<img src="assets/img/peapods_simul.png" style='margin-top:-1%;'/>
</div>
</center>

<!-- ***

<center>__Simulation of $S(Q,\omega)$ in the TOF geometry__</center>

<center>
<iframe width=1100 height=700  src="assets/img/plot_SQW.html"></iframe>
</center> -->


--- &vertical 

## Carbon nano-peapods: a model 1D system
<div style='float:left;width:45%'>
<img src="assets/img/L_T_peapods_TG.png" style='margin-top:10%'/>
<img src="assets/img/rotations.png"/>
</div>
<div style='float:right;width:55%'>
   <br>
   <h3 style="color:red">High T: Progressive fusion</h3>
   <ul> 
      <li>First observation of the fusion of a Takahasi-Gürsey chain</li>
      <li> No elastic diffusion $\Leftrightarrow$ Positionnal and orientationnal disorders</li>
   </ul>
   <p style='text-align:left'>
   <a href="http://prb.aps.org/abstract/PRB/v87/i19/e195438" style='font-style:italic'>Bousige et al., PRB <b>87</b> (2013) 195438</a><br>
   <a href="http://prb.aps.org/abstract/PRB/v86/i4/e045446" style='font-style:italic'>Bousige et al., PRB <b>86</b> (2012) 045446</a><br>
   <span style="color:red">In</span>elastic nature:</p><br>
   <img align:"top" src="assets/img/nature_el.png" style='float:left;height:300px;'/><img src="assets/img/peapods_kis_THT2.png" style="width:60%;" />
</div>

***

<center>__At higher temperature: loss of the elastic part__</center>


<div style='float:left;width:40%;margin-left:5%'>
<img src="assets/img/peapods_translations_INS.png" style='margin-top:30%'/>
</div>
<div style='float:right;width:40%;margin-right:5%;'>
<img src="assets/img/peapods_kis_THT.png"/>
</div>


--- 

## Carbon nano-peapods: a model 1D system
### Quick summary of the behavior for T > 300 K

<center>
<img src="assets/img/peapods_TEM2.png" style='margin-top:10%;margin-bottom:7%;width:40%;'/> = 
<img src="assets/img/peapods_ressort.png" style='margin-top:5%;width:50%;'/> 
</center>


--- 

## Carbon nano-peapods: a model 1D system
### <span style="color:#7c7cfd">Low temperatures</span>

<img src="assets/img/L_T_peapods.png" style='margin-top:5%;width:55%'/>

--- &twocol w1:40%;margin-left:5% w2:27%;margin-right:10%;

## Carbon nano-peapods: a model 1D system
### Rotation-translation coupling and orientationnal ordering

***=left
<img src="assets/img/peapods_rotations_INS.png" style='margin-top:10%'/>
<img align:"top" src="assets/img/nature_el.png" style='width:35%;margin-top:2%'/>

***=right
<img src="assets/img/rot_struct.png" style='margin-top:2%'/>


--- dt:fade &twocol w1:40%;margin-left:10% w2:27%;margin-right:10%;

## Carbon nano-peapods: a model 1D system
### Rotation-translation coupling and orientationnal ordering

***=left
<img src="assets/img/peapods_schema.png" style='margin-top:1%'/>
<center class="fragment current-visible">
Still :
<img src="assets/img/peapods_TEM2.png" style='width:40%;'/> = 
<img src="assets/img/peapods_ressort.png" style='width:50%;'/> 
</center>

***=right
<img src="assets/img/rot_struct.png" style='margin-top:2%'/>


--- &twocol w1:40%;margin-left:5%;margin-top:5% w2:50%;margin-top:5%

## Carbon nano-peapods: a model 1D system... _for friction_
### Let's have a look at the translations

***=left
<img src="assets/img/peapods_translations_INS.png"/>

***=right
<img src="assets/img/Experimental_nofit.png"/>


--- &twocol w1:40%;margin-left:1%;margin-top:5% w2:50%;margin-top:5%

## Carbon nano-peapods: a model 1D system... _for friction_
### Let's have a look at the translations

***=left
In the case of a perfectly plane buckypaper and a TAS, one can show:
$$\frac{S(Q,\omega)}{k_BT} \propto \frac{NF^2(Q)}{4\hbar mc^2}\int\limits_{0}^{2\pi}\text{d}\phi_t\frac{(Q\cos\phi_t)^2}{\sin^2\left(\frac{Q L}{2}\cos\phi_t\right)}\times$$
$$\quad\quad\quad\quad\quad\delta\left(\omega \pm \frac{2c}{L}\left|\sin\left(\frac{Q L}{2}\cos\phi_t\right)\right|\right)$$


***=right
<img src="assets/img/Experimental_nofit.png"/>

--- &twocol w1:40%;margin-left:1%;margin-top:5% w2:50%;margin-top:5%

## Carbon nano-peapods: a model 1D system... _for friction_
### Let's have a look at the translations

***=left
In the case of a perfectly plane buckypaper and a TAS, one can show:
$$\frac{S(Q,\omega)}{k_BT} \propto \frac{NF^2(Q)}{4\hbar mc^2}\int\limits_{0}^{2\pi}\text{d}\phi_t\frac{(Q\cos\phi_t)^2}{\sin^2\left(\frac{Q L}{2}\cos\phi_t\right)}\times$$
$$\quad\quad\quad\quad\quad\delta\left(\omega \pm \frac{2c}{L}\left|\sin\left(\frac{Q L}{2}\cos\phi_t\right)\right|\right)$$
<br>
- With an experimental resolution

***=right
<img src="assets/img/Experimental_fitHT.png"/>


--- &twocol w1:40%;margin-left:1%;margin-top:5% w2:50%;margin-top:5%

## Carbon nano-peapods: a model 1D system... _for friction_
### Let's have a look at the translations

***=left
In the case of a perfectly plane buckypaper and a TAS, one can show:
$$\frac{S(Q,\omega)}{k_BT} \propto \frac{NF^2(Q)}{4\hbar mc^2}\int\limits_{0}^{2\pi}\text{d}\phi_t\frac{(Q\cos\phi_t)^2}{\sin^2\left(\frac{Q L}{2}\cos\phi_t\right)}\times$$
$$\quad\quad\quad\quad\quad\delta\left(\omega \pm \frac{2c}{L}\left|\sin\left(\frac{Q L}{2}\cos\phi_t\right)\right|\right)$$
<br>
- With an experimental resolution
- __And a damping parameter $\gamma$__


***=right
<img src="assets/img/Experimental_fit.png"/>


--- &vertical

## Carbon nano-peapods: a model 1D system... _for friction_
### Appearance of friction at low temperatures

<div style='float:left;width:55%;'>
<img src="assets/img/gammas_ampl_T.png" style="margin-top:10%" />
</div>
<div style='float:right;width:30%;margin-right:10%;'>
<br><br>
Fit by a power law :
<br><br>
$$
\left\lbrace
\begin{array}{rll}
\gamma=&0 &\text{ for } T > T_C\\
\gamma=&\left(T_C-T\right)^{\chi}&\text{ for } T \le T_C
\end{array}\right.
$$
<br>
${T_C}=230$ K and $\chi=0.75$
<br>
<br>
<blockquote style='background:rgba(0,0,0,0);color:black;width:100%;box-shadow: 3px 0px 5px rgba(0,0,0,0.9);' class="fragment fade-in">
Now :
<img src="assets/img/peapods_TEM2.png" style='width:90%;'/> = 
<img src="assets/img/peapods_friction.png" style="width:100%;"/>
</blockquote>
</div>

***

### No 3D ordering
<center>No symmetrization of the 1D periodicity diffraction peak</center>
<img src="assets/img/peapods_comparison_xp_simul.png" style="width:35%;margin-top:1%"/>


--- dt:fade &twocol w1:40% w2:60%;margin-top:5%

## Carbon nano-peapods: a model 1D system... _for friction_
### Summary

***=left
<img src="assets/img/peapods_resume.png" style="width:90%;margin-top:2%"/>

***=right
<center>$T>T_C$ : ~~Friction~~<br><br></center>
- Link to super-lubricity observed for water flowing through CNT?
<br><br><br><br><br>
<center>$T\le T_C$ : Friction<br><br></center>
- Transition by breaking of analyticity? <br>(Aubry transition in a Frenkel-Kontorova model)
- Role of the RBM?
- Role of the longitudinal CNT modes as in water diffusion?
<br><br>
$\Rightarrow$ MD simulations still running...


<!-- --- dt:fade

## Conclusions

<br>
<center>
<b>Very different systems, and yet:</b>
<div style='line-height:1.5em;width:80%;'>
<ul>
   <li><b>Similar problematics :</b></li>
   <ul>
      <li>Carbon compounds</li>
      <li>Study of the structure</li>
      <li>Study of the dynamics</li>
      <li>Trying to draw a link between local and macroscopic behaviors</li>
   </ul>
<li><b>Similar techniques:</b></li>
   <ul>
      <li>Diffraction (x-rays, neutrons)</li>
      <li>Inelastic Neutrons Scattering</li>
      <li>Raman spectroscopy</li>
      <li>Monte Carlo and Molecular Dynamics Simulations </li>
      <li>Playing on pressure, temperature...</li>
   </ul>
</ul>
</div>
</center>
 -->

--- &twocol w1:30%;margin-left:30% w2:40%

# Thanks!
<br><br>
<br><br>

***=left


__ILL and LPS__

Pascale Launois

Stéphane Rols


***=right

__ILM__

Alfonso san Miguel

Denis Machon

