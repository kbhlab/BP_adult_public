FOLDERS

- partial_data - data downloads from partway through the data collection process - either to start coding exclusion criteria or to check eligible N
- raw_data - final dataset, raw data downloaded from online testing platform
- files not in either folder have been manipulated in someway by researchers



NOTE ON FILES

*_start files have the "datetime" info taken from the .Rda files in the raw_data folder

This info is then mapped on to the "Start Time" column in the full_meta_*_start files in order to allow joins between meta data and responses during experiment.

This step had to be completed by hand as the "datetime" and "Start Time" info did not match exactly. It could be different by several seconds or minutes.

The majority of the time this was no issue, if only one participant had completed the experiment on a given date.
If multiple participants completed the study on one day, the start times were matched by hour and minute.
In cases where a final decision could not be made based on time, the Worker ID was cross referenced to the subject log.

LHQ Data Dictionary has info on what questions were used in the LHQ and how the questions were renamed for coding from both the original and follow-up LHQ.
Follow-ups were sent when a participant did not complete the LHQ correctly (e.g., reported number of total years instead of AoA). These error codes are also present in data dictionary