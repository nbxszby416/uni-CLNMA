# Statement on reproducing results for "Network meta-analysis made simple: a composite likelihood approach"

For reproducibility of the results presented in our manuscript, please find the functional example of the proposed method at the following link: 
https://github.com/nbxszby416/uni-CLNMA/tree/main/RSM_12-2023-0247_Reproducibility

Below are the details of the files and functions used to generate each figure: 

* Figure 1: Illustrates the diagrams of evidence network, where node sizes are proportional to the number of participants assigned to each treatment. Solid lines represent direct comparisons between treatments in trials, with the thickness of the lines proportional to the number of trials directly comparing each pair of treatments. Please refer to 
<span style="color:blue">
Figure 1/Figure1_network_plot.do.
</span>

* Figure 2: Compares computational time for the proposed method with two existing methods implemented in the R packages ‘gemtc’ and ‘netmeta’, with varying numbers of treatments and studies. Please refer to 
<span style="color:blue">
Figure 2/Figure2_comparison_time.R.
</span>

* Figure 3: Displays coverage probabilities of estimated pooled treatment effects for comparisons between treatments AB and AC using the proposed method, with and without the KC-corrected and MD-corrected sandwich variance estimators under (a) within-study correlation of 0.2, and (b) within-study correlation of 0.5. Please refer to 
<span style="color:blue">
Figure 3/Figure3_CP.R.
</span>

* Figure 4: Compares overall relative treatment estimates with 95\% confidence intervals using the pairwise meta-analysis approach, the standard NMA based on the Lu and Ades’ approach, the proposed method without corrections, and the proposed method with the KC-corrected or MD-corrected sandwich variance estimators. Each node represents the pooled mean difference for the outcomes of interest. Please refer to 
<span style="color:blue">
Figure 4/Figure4_forest.R.
</span>

* Figure 5: Compares Z values using the standard NMA based on the Lu and Ades’ approach, the proposed method without corrections, and the proposed method with the KC-corrected or MD-corrected sandwich variance estimators, respectively. Please refer to 
<span style="color:blue">
Figure 5/Figure5_scatter.R.
</span>

* Figure S3: Displays treatment rankings calculated based on the surface under the cumulative ranking (SUCRA) method. Please refer to 
<span style="color:blue">
Figure S3/FigureS3_sucra.R.
</span>