<center><h1>FAQ</h1></center>

- <h4> <b>Q: What are the criteria for BEST to include datasets?</b></h4>
  
   <p style="text-align:justify;"><b>A:</b> BEST is committed to identifying robust tumor biomarkers through large-scale data. Hence, we retrieved cancer datasets with both expression data and important clinical information (e.g., survival, therapy, etc.) as much as possible. Eligible datasets were mainly enrolled from five databases, including The Cancer Genome Atlas Program (<a href="https://portal.gdc.cancer.gov" target="_blank">TCGA</a>), Gene Expression Omnibus ( <a href="https://www.ncbi.nlm.nih.gov/geo/" target="_blank">GEO</a>), International Cancer Genome Consortium (<a href="https://dcc.icgc.org" target="_blank">ICGC</a>), Chinese Glioma Genome Atlas (<a href="http://www.cgga.org.cn/" target="_blank">CGGA</a>), and <a href="https://www.ebi.ac.uk/arrayexpress/" target="_blank">ArrayExpress</a>.</p>

<br/>

- <h4><b>Q: How the data is reannotated?</b> </h4>

  __A__: Data were re-annotated if the original probe sequences were available based on the GRCh38 reference from [GENCODE](https://www.gencodegenes.org/).

<br/>

- <h4><b>Q: How the data is processed?</b></h4>

  <p style="text-align:justify;"><b> A: </b>For RNA-seq data, raw count read was converted to transcripts per kilobase million (TPM) and further log-2 transformed. The raw microarray data from Affymetrix®, Illumina®, and Agilent® were processed using the <i>affy</i>, <i>lumi</i>, and <i>limma</i> packages, respectively. The normalized matrix files were directly downloaded for microarray data from other platforms. Gene expression was further transformed into z-score across patients in each dataset. To make it easier for users to interpret and present analysis results, we cleaned and unified the clinical traits. Take KRAS mutation as an example, GSE39084 named it 'kras.gene.mutation.status', 'mutation' was labeled 'M' and 'wild type' was labeled 'WT'; whereas GSE143985 named it 'kras_mutation', 'mutation' was labeled 'Y' and 'wild type' was labeled 'N'. We uniformly termed it 'KRAS', and ‘mutation’ was labeled 'Mut' and 'wild type' was labeled 'WT'.</p>

<br/>



