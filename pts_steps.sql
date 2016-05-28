/* Sample functions
 * reference: http://www.onlamp.com/pub/a/onlamp/2006/05/11/postgresql-plpgsql.html?page=2
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
*/

/*
 * test function for learning PL/PgSQL
 */
CREATE TABLE example_data.test(
    id integer
);
INSERT INTO example_data.test(id) values(1);
INSERT INTO example_data.test(id) values(2);
INSERT INTO example_data.test(id) values(3);
INSERT INTO example_data.test(id) values(4);
INSERT INTO example_data.test(id) values(5);

DROP FUNCTION test_function(text);

CREATE OR REPLACE FUNCTION test_function (
) RETURNS integer AS $$
DECLARE
    num record;
    ret integer := 0;
    counter integer;
BEGIN
	SELECT INTO counter count(id)
	FROM example_data.test;
	
	WHILE counter - 1 > 0 LOOP
    	RAISE NOTICE 'counter = %', counter;
    	counter := counter - 1;
    	RAISE NOTICE 'ret = %', ret;
    	ret := ret + 1;
	END LOOP;

    RETURN ret;
END
$$ LANGUAGE plpgsql;

SELECT test_function();


-- table to store the steps with their length
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

) RETURNS geometry AS $$
DECLARE
    line geometry;
    dist float;
BEGIN
	FOR i IN example_data.continental.geom LOOP
	INSERT INTO example_data.steps (
	);
END
$$ LANGUAGE plpgsql;


SELECT steps.startpt, step, st_length(st_transform(step, 3395)) AS geom_length
FROM (
    SELECT 
        a.gid AS startid,
        b.gid,
        st_makeline(a.geom, b.geom) AS step
    FROM 
        example_data.continental AS a
        INNER JOIN example_data.continental AS b ON a.gid + 1 = b.gid
    ORDER BY 
        a.gid
) AS steps;



