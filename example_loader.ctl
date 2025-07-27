-- Oracle Loader Control File Example
-- This file demonstrates the basic structure of a SQL*Loader control file

LOAD DATA
INFILE 'data.csv'
BADFILE 'data.bad'
DISCARDFILE 'data.dsc'
APPEND
INTO TABLE employees
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    employee_id,
    first_name,
    last_name,
    email,
    hire_date DATE "YYYY-MM-DD",
    salary,
    department_id
) 