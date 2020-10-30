# Toxicity Benchmarks for Shellfish

EPA. 2000. "Guidance for Assessing
Chemical Contaminant Data for Use in Fish Advisories
Volume 2
Risk Assessment and Fish Consumption Limits
Third Edition

Table 5-2 gives "Toxicity Equivalency Factors" for PAHs.  These relate assumed
toxic by order of magnitude factors to Benzo(a)Pyrene, one of the more
carcinogenic PAHs.  Using them to generate a weighted sum can produce "TEQs"
that provide relative toxicity of a mixture of toxic chemicals.

In general, risk levels are expressed in terms of daily doses in mg/kg-d
Several individual PAHs have established reference doses (RfDs), but confidence
in those levels is low.

NOAELs ("No Adverse Observable Effects Levels") are sometimes also available,
but not offered here.

Values for individual PAHs tend to hover around 10^-2 t0 10^-1 mg/kg or slightly
higher.

But We can't go from those to acceptable levels in shellfish without estimating
the size of shellfish meats, number of shellfish consumed at a sitting, size of
human, etc.

-----------------------------------
We can guesstimate, based on information found from unverifiable sources on-line:
One pound of mussels (let's call it 500gms) is ~ 20 to 25 animals, and a fairly
typical serving.  It might have on the order of 20% meat, so total consumed meat
in a serving might be on the order of 100g.

Maine:  An Encyclopedia" (https://maineanencyclopedia.com/blue-mussel-harvests/)
suggests meat averages about 17% of weight of whole mussels in Maine.  Close
enough to 20% for rough estimates.

so, if a 60 kg (~ 130 lb) human consumed a regular serving

$dose = conc in ng/g / 1000mg/ng * 100 g / (60kg * 1000g/kg)$

we're seeing total PAH concentrations in the most contaminated samples from
Portland Harbor on the order of 11500 ng/g, so 

$dose = 11500 / 1000 * 100 / (60000) = 0.019  mg/kg$

This is on the low end of levels of concern, but not entirely outside them.  So
our mos toxic mussels probably DO reach levels of concern.  But just barely.