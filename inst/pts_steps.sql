/*
 * Creates steps
 */
INSERT INTO steps (
    step,
    date,
    dt,
    b_name
) (
    SELECT 
        st_makeline(a.relocation, b.relocation) AS step,
        a.date,
        b.date - a.date AS dt,
        a.b_name
    FROM 
        relocs_temp AS a
        INNER JOIN relocs_temp AS b 
            ON a.r_id + 1 = b.r_id AND
            a.b_name = b.b_name
    WHERE a.b_name = 'migrating animal'--here comes i
    ORDER BY a.r_id
);

