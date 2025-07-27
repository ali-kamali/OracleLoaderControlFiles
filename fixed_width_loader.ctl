-- Fixed-Width Oracle Loader Control File Example
-- Demonstrates loading data from fixed-width format files

LOAD DATA
INFILE 'fixed_width_data.txt'
BADFILE 'fixed_width.bad'
DISCARDFILE 'fixed_width.dsc'
REPLACE
INTO TABLE customer_data
(
    customer_id POSITION(1:5) INTEGER EXTERNAL,
    customer_name POSITION(6:35) CHAR(30),
    address_line1 POSITION(36:65) CHAR(30),
    address_line2 POSITION(66:95) CHAR(30),
    city POSITION(96:115) CHAR(20),
    state POSITION(116:117) CHAR(2),
    zip_code POSITION(118:127) CHAR(10),
    phone POSITION(128:137) CHAR(10),
    email POSITION(138:167) CHAR(30),
    registration_date POSITION(168:177) DATE "YYYY-MM-DD",
    status POSITION(178:178) CHAR(1)
) 