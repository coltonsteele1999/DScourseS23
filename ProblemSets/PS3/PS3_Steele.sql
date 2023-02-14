CREATE TABLE insurance_data (
 policyID INTEGER PRIMARY KEY,
 statecode TEXT NOT NULL,
 county TEXT NOT NULL,
 eq_site_limit DECIIMAL  NOT NULL,
 hu_site_limit DECIMAL  NOT NULL,
 fl_site_limit DECIMAL  NOT NULL,
 fr_site_limit DECIMAL  NOT NULL,
 tiv_2011 FLOAT NOT NULL,
 tiv_2012 FLOAT NOT NULL,
 eq_site_deductible DECIMAL  NOT NULL,
 hu_site_deductible DECIMAL  NOT NULL,
 fl_site_deductible DECIMAL  NOT NULL,
 fr_site_deductible DECIMAL  NOT NULL,
 point_latitute DECIMAL  NOT NULL,
 point_longitude DECIMAL NOT NULL,
 line TEXT NOT NULL,
 construction TEXT NOT NULL,
 point_granularity DECIMAL  NOT NULL
);

.mode csv
.separator ","
.import FL_insurance_sample.csv insurance_data


SELECT * FROM insurance_data LIMIT 10 ;

SELECT DISTINCT county FROM insurance_data ;

SELECT AVG(tiv_2012 - tiv_2011) AS avg_diff FROM insurance_data ;

SELECT construction, COUNT(*) AS count, COUNT(*) * 1.0 / (SELECT COUNT(*) FROM insurance_data) AS fraction
FROM insurance_data
GROUP BY construction ;
