CREATE OR REPLACE FUNCTION fib (fib_for integer
) RETURNS integer AS $$
BEGIN
IF fib_for < 2 THEN
RETURN fib_for;
END IF;
RETURN fib(fib_for - 2) + fib(fib_for - 1);
END;
$$LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION fib_fast(
     fib_for integer
) RETURNS integer AS $$
DECLARE
    ret integer := 0;
    nxt integer := 1;
    tmp integer;
BEGIN
   FOR num IN 1..fib_for LOOP
        tmp := ret;
       ret := nxt;
       nxt := tmp + nxt;
   END LOOP;

   RETURN ret;
END;
$$ LANGUAGE plpgsql;


CREATE TABLE example_data.steps (
    gid integer PRIMARY KEY,
    step geometry,
    len float
);

/*
 * pts_steps
 * Calculates steps from subsequent relocations
 * Input: point geometry field
 * Output: line geometry field
 */
CREATE OR REPLACE FUNCTION pts_steps (
    pts geometry
) RETURNS geometry AS $$
DECLARE
    line geometry;
    dist float;
BEGIN
	
	INSERT INTO example_data.steps (
	   
	)
	

DROP FUNCTION test_function(integer);

CREATE OR REPLACE FUNCTION test_function (
    rep integer
) RETURNS integer AS $$
DECLARE
    ret integer := 0;
BEGIN
	FOR num IN 1..rep LOOP
    	RAISE NOTICE 'ret = %', ret;   
    	ret := ret + 1;
	END LOOP;
	
	RETURN ret;
END
$$ LANGUAGE plpgsql;

SELECT test_function(5);
