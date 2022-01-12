# SWAT_Mussels

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

Analysis of data on tissue concentrations of toxics from Maine's Surface Water
Ambient Toxics program.

Data is from DEP's long-running program sampling tissue of various shellfish for
concentrations of selected persistent toxic chemicals, including metals, PCBs,
PAHs, and certain pesticides.  Our abalysis focuses on concetratins of toxic
contaminants in blue mussels (*Mytilus edulis*).

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our State of the Bay data
archives ensure the science underlying the 2020/2021 State of Casco Bay report
is documented and reproducible by others. The purpose of these archives is to
release  data and data analysis code whenever possible to allow others to
review, critique, learn from, and build upon CBEP science.

# Archive Structure
CBEP 2020/2021 State of the Bay data analysis summaries contain a selection of 
data,  data analysis code, and visualization code as used to produce 
results shared via our most recent State of Casco Bay report. Usually, these
archives are organized into two or three folders, including the following:

- Data.  Contains data in simplified or derived form as used in our
data  analysis.  Associated metadata is contained in related Markdown documents,
usually `DATA_SOURCES.md` and `DATA_NOTES.md`.

- Analysis.  Contains one or more R Notebooks proceeding through the principal
data analysis steps that underpin SoCB reporting. To simplify the archives,
much preliminary analysis, and many analysis "dead ends" have been omitted. 

- Graphics.  Contains R Notebooks stepping through development of graphics, and
also copies of resulting graphics, usually in \*.png and \*.pdf formats.  These
graphics may differ from graphics as they appear in final State of the Bay
graphical layouts. Again, most draft versions of graphics have been omitted for 
clarity.

# Summary of Data Source
Data is derived from the EGAD database maintained by Maine's Department of
Environmental Protection (DEP).  Contents of the data shared with CBEP included
tissue toxics samples from shellfish collected from locations within Casco Bay.
Following practice in prior SoCB Reports, our analysis focused only on toxics
in blue mussels (*Mytilus edulis*).
