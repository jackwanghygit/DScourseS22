-- ******************
-- Import data
-- ******************
.print ' '
.print 'Importing data'
-- First, create the table that the CSV will be stored in
CREATE TABLE "insurance" (
"policyID" INTEGER,
"statecode" CHAR,
"county" CHAR,
"eq_site_limit" INTEGER,
"hu_site_limit" INTEGER,
"fl_site_limit" INTEGER,
"fr_site_limit" INTEGER,
"tiv_2011" INTEGER,
"tiv_2012" INTEGER,
"eq_site_deductible" INTEGER,
"hu_site_deductible" INTEGER,
"fl_site_deductible" INTEGER,
"fr_site_deductible" INTEGER,
"point_latitude" INTEGER,
"point_longitude" INTEGER,
"line" INTEGER,
"construction" INTEGER,
"point_granularity" INTEGER
);

-- Tell SQL to expect a CSV file by declaring CSV mode
.mode csv

-- Next, import the CSV following the directions on the sqlitetutorial website
.import FL_insurance_sample.csv insurance



-- ******************
-- View first 10 observations
-- ******************
.print ' '
.print 'View first 10 observations'
-- View first 10 observations
SELECT * FROM insurance LIMIT 10;



-- ******************
-- List which counties are in the sample?
-- ******************
.print ' '
.print 'Counties'
-- List all the counties
SELECT DISTINCT county FROM insurance;




-- ******************
-- Average property appreciation
-- ******************
.print ' '
.print 'Average property appreciation'
SELECT  AVG(tiv_2012-tiv_2011) as apa FROM basketball;


-- ******************
-- Distribution of construction materials
-- ******************
.print ' '
.print 'Construction materials distribution'
-- Frequency table of construction
SELECT construction, COUNT(*) FROM insurance GROUP BY construction;


-- ******************
-- Save as text file
-- ******************
.output PS3_Wang.sqlite3
.dump


-- ProTip: You can execute this file from the Linux command line by issuing the following:
-- sqlite3 < SQLexample.sql > SQLexample.sqlog
