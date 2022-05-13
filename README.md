# Adjusted Professional Golf Scores

Following the methods of <a href="https://datagolfblogs.ca/a-predictive-model-of-tournament-outcomes-on-the-pga-tour/"> Data Golf (2017)</a> 
and <a href="http://www.columbia.edu/~mnb2/broadie/Assets/owgr_20120507_broadie_rendleman.pdf"> Broadie and Rendelman (2012)</a>, 
scores are adjusted for tournament-round difficulties. To obtain estimates of tournament-round difficulties, we run the following fixed effects regression model:
<br>
<br>
<center><em>Score<sub>ij</sub> = &mu;<sub>i</sub> + &delta;<sub>j</sub> + &epsilon;<sub>ij</sub></em></center> 
<br>
where <em>i</em> indexes a player and <em>j</em> indexes a tournament round. 
We then collect the <em>&delta;<sub>j</sub></em> coefficients from the model and define the adjusted score as <em>Score<sub>ij</sub> - &delta;<sub>j</sub></em>. 

