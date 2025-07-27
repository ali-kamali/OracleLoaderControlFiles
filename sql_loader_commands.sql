-- SQL Script to create tables and demonstrate SQL*Loader usage
-- Run this script in your Oracle database before using the control files

-- Create employees table for basic and advanced examples
CREATE TABLE employees (
    employee_id NUMBER(6) PRIMARY KEY,
    first_name VARCHAR2(50),
    last_name VARCHAR2(50),
    email VARCHAR2(100),
    phone_number VARCHAR2(20),
    hire_date DATE,
    job_id VARCHAR2(10),
    salary NUMBER(8,2),
    commission_pct NUMBER(2,2),
    manager_id NUMBER(6),
    department_id NUMBER(4)
);

-- Create customer_data table for fixed-width example
CREATE TABLE customer_data (
    customer_id NUMBER(5) PRIMARY KEY,
    customer_name VARCHAR2(30),
    address_line1 VARCHAR2(30),
    address_line2 VARCHAR2(30),
    city VARCHAR2(20),
    state VARCHAR2(2),
    zip_code VARCHAR2(10),
    phone VARCHAR2(10),
    email VARCHAR2(30),
    registration_date DATE,
    status CHAR(1)
);

-- Grant necessary permissions (run as SYSTEM or DBA)
-- GRANT CREATE ANY DIRECTORY TO your_username;
-- CREATE OR REPLACE DIRECTORY data_dir AS '/path/to/your/data/files';

-- Example SQL*Loader commands to run from command line:
/*
Basic loading:
sqlldr username/password@database control=example_loader.ctl

Advanced loading with log file:
sqlldr username/password@database control=advanced_loader.ctl log=advanced_loader.log

Fixed-width loading:
sqlldr username/password@database control=fixed_width_loader.ctl log=fixed_width_loader.log

With additional parameters:
sqlldr username/password@database control=example_loader.ctl log=loader.log bad=loader.bad discard=loader.dsc rows=1000 direct=true
*/

-- Verify loaded data
SELECT COUNT(*) FROM employees;
SELECT * FROM employees ORDER BY employee_id;

SELECT COUNT(*) FROM customer_data;
SELECT * FROM customer_data ORDER BY customer_id; 