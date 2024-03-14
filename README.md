Task: Energy Rating Data for Air Conditioners
Background:
The Energy Rating Label (ERL) is a mandatory comparative energy label that provides
consumers with product energy performance information at point-of-sale on a range of
appliances. Attached to each appliance, allows comparison between similar appliance
models through a star rating system (the greater the number of stars, the higher the
efficiency) and the annual energy consumption. Further details are available from
http://www.energyrating.gov.au
Data has been collected from suppliers when they register appliances that are going to
be sold in Australia and New Zealand, available from
http://data.gov.au/dataset/energy-rating-for-household-appliances.
A new star rating system has been used since 2010, but the data also includes values
for the previous SRI system used prior to 2010. Estimated yearly power consumption is
also provided for each appliance. Some fields such as Model Number/name will not be
relevant for this data mining task, other than for labelling purposes. Some fields may
require pre-processing.


A factor that distinguishes air conditioners is that, unlike most other appliances, have
two ratings: one for their efficiency when cooling and the other with their efficiency as
heating. (Though, not all models can cool and heat.)
Requirements:
1. Download the latest airconditioning dataset from the above website, and restrict to
models that are available (using the Available Status field) and to models that are
sold in Australia. You can identify models that are sold in Australia by whether
“Australia” appears in the Sold in field. 

2. You are required to propose a model for predicting the SRI values (quantitative)
of air conditioners according to the new star rating system used since 2010, using
other relevant variables apart from the star ratings. Do this for both their cooling
and their heating ratings.

3. Also, propose a model to predict the yearly energy consumption of the air condi-
tioner in heating and a model to predict it in cooling.

4. Also, describe, with summaries, visualisations, and by other means, the relation-
ship, if any, between an air conditioner’s rating when cooling and its rating when

heating. Is there evidence to believe that the relationship holds after accounting
for the size, design, maker, and other properties of the AC?
5. Discuss any data preprocessing and selection of attributes which have been applied.
6. You need to provide the performance measures of your prediction results.
7. Present all of your models, with particular attention to informing consumers about
important factors which affect energy efficiency.
8. Discuss the variables which are most important for the purpose of predicting energy
rating.
