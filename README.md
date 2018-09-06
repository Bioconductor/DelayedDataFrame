# DelayedDataFrame

Based on the standard DataFrame metaphor, we are trying to implement
the feature of delayed operation on the DelayedDataFrame, with a slot
of lazyIndex, which saves the mapping indexes for each column of
DelayedDataFrame. Methods like show, validity check, [/[[ subsetting,
rbind/cbind are implemented for DelayedDataFrame to be operated around
lazyIndex. The listData slot stays untouched until a realization call
e.g., DataFrame constructor OR as.list() incurred.
