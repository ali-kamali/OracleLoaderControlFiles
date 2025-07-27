-- Advanced Oracle Loader Control File Example
-- Demonstrates conditional loading, transformations, and multiple input files

LOAD DATA
INFILE 'employees_data.csv'
BADFILE 'employees.bad'
DISCARDFILE 'employees.dsc'
APPEND
INTO TABLE employees
WHEN (1:3) = 'EMP'
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    employee_id INTEGER EXTERNAL,
    first_name CHAR(50),
    last_name CHAR(50),
    email CHAR(100),
    phone_number CHAR(20),
    hire_date DATE "YYYY-MM-DD",
    job_id CHAR(10),
    salary DECIMAL EXTERNAL,
    commission_pct DECIMAL EXTERNAL,
    manager_id INTEGER EXTERNAL,
    department_id INTEGER EXTERNAL
)

-- Load managers separately
INFILE 'managers_data.csv'
BADFILE 'managers.bad'
DISCARDFILE 'managers.dsc'
APPEND
INTO TABLE employees
WHEN (1:3) = 'MGR'
FIELDS TERMINATED BY ','
OPTIONALLY ENCLOSED BY '"'
TRAILING NULLCOLS
(
    employee_id INTEGER EXTERNAL,
    first_name CHAR(50),
    last_name CHAR(50),
    email CHAR(100),
    phone_number CHAR(20),
    hire_date DATE "YYYY-MM-DD",
    job_id CHAR(10),
    salary DECIMAL EXTERNAL,
    commission_pct DECIMAL EXTERNAL,
    manager_id INTEGER EXTERNAL,
    department_id INTEGER EXTERNAL
) 