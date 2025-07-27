-- Oracle Table DDL Generated from SQL*Loader Configuration
-- Generated on: 2024-01-15 15:30:00
-- Table: EMPLOYEES
-- Character Set: CHARACTERSET AL32UTF8

CREATE TABLE EMPLOYEES (
    EMPLOYEE_ID NUMBER(6) NOT NULL,
    FIRST_NAME VARCHAR2(50),
    LAST_NAME VARCHAR2(50),
    EMAIL VARCHAR2(100),
    PHONE_NUMBER VARCHAR2(20),
    HIRE_DATE DATE,
    JOB_ID VARCHAR2(10),
    SALARY NUMBER(8,2),
    COMMISSION_PCT NUMBER(2,2),
    MANAGER_ID NUMBER(6),
    DEPARTMENT_ID NUMBER(4),
    STATUS CHAR(1) DEFAULT 'A',
    CREATED_DATE DATE,
    MODIFIED_DATE DATE
);

-- Add comments for better documentation
COMMENT ON COLUMN EMPLOYEES.EMPLOYEE_ID IS 'Unique employee identifier';
COMMENT ON COLUMN EMPLOYEES.FIRST_NAME IS 'Employee first name';
COMMENT ON COLUMN EMPLOYEES.LAST_NAME IS 'Employee last name';
COMMENT ON COLUMN EMPLOYEES.EMAIL IS 'Employee email address';
COMMENT ON COLUMN EMPLOYEES.PHONE_NUMBER IS 'Employee phone number';
COMMENT ON COLUMN EMPLOYEES.HIRE_DATE IS 'Employee hire date';
COMMENT ON COLUMN EMPLOYEES.JOB_ID IS 'Job identifier';
COMMENT ON COLUMN EMPLOYEES.SALARY IS 'Employee salary';
COMMENT ON COLUMN EMPLOYEES.COMMISSION_PCT IS 'Commission percentage';
COMMENT ON COLUMN EMPLOYEES.MANAGER_ID IS 'Manager employee ID';
COMMENT ON COLUMN EMPLOYEES.DEPARTMENT_ID IS 'Department identifier';
COMMENT ON COLUMN EMPLOYEES.STATUS IS 'Employee status (A=Active, I=Inactive)';
COMMENT ON COLUMN EMPLOYEES.CREATED_DATE IS 'Record creation date';
COMMENT ON COLUMN EMPLOYEES.MODIFIED_DATE IS 'Record modification date';

-- Table is configured for partition: EMPLOYEES_2024
-- Note: Partition DDL must be created separately based on your partitioning strategy

-- Performance Recommendations:
-- 1. Consider adding appropriate indexes based on query patterns
-- 2. Review storage parameters for your data volume
-- 3. Consider partitioning for large tables
-- 4. Monitor table growth and adjust storage accordingly

-- Suggested indexes for common queries:
-- CREATE INDEX idx_employees_dept_id ON EMPLOYEES(DEPARTMENT_ID);
-- CREATE INDEX idx_employees_manager_id ON EMPLOYEES(MANAGER_ID);
-- CREATE INDEX idx_employees_hire_date ON EMPLOYEES(HIRE_DATE);
-- CREATE INDEX idx_employees_email ON EMPLOYEES(EMAIL);

-- Suggested primary key:
-- ALTER TABLE EMPLOYEES ADD CONSTRAINT pk_employees PRIMARY KEY (EMPLOYEE_ID);

-- Suggested foreign keys:
-- ALTER TABLE EMPLOYEES ADD CONSTRAINT fk_employees_manager 
--     FOREIGN KEY (MANAGER_ID) REFERENCES EMPLOYEES(EMPLOYEE_ID);
-- ALTER TABLE EMPLOYEES ADD CONSTRAINT fk_employees_department 
--     FOREIGN KEY (DEPARTMENT_ID) REFERENCES DEPARTMENTS(DEPARTMENT_ID); 