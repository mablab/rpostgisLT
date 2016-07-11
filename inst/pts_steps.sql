/*
 * Creates steps
 */
INSERT INTO steps (
    r_rowname,
    reloc1,
    step,
    date,
    dt,
    b_name
) (
    SELECT
    CASE 
        WHEN a.pkey IS NOT NULL THEN a.pkey
        WHEN a.pkey IS NULL THEN a.r_id::text
    END AS r_rowname,
    CASE
        WHEN a.relocation IS NOT NULL AND b.relocation IS NOT NULL THEN a.relocation
        WHEN (a.relocation IS  NOT NULL AND b.relocation IS NULL) THEN a.relocation
        WHEN (a.relocation IS NULL AND b.relocation IS NOT NULL) OR 
             (a.relocation IS NULL AND b.relocation IS NULL) THEN NULL
    END as reloc1,
    CASE
        WHEN a.relocation IS NOT NULL AND b.relocation IS NOT NULL THEN st_makeline(a.relocation, b.relocation)
        WHEN (a.relocation IS  NOT NULL AND b.relocation IS NULL) THEN NULL
        WHEN (a.relocation IS NULL AND b.relocation IS NOT NULL) OR 
             (a.relocation IS NULL AND b.relocation IS NULL) THEN NULL
    END as step,
        a.date,
        b.date - a.date AS dt,
        'A153'
    FROM 
        relocs_temp AS a
        INNER JOIN relocs_temp AS b 
            ON a.r_id + 1 = b.r_id AND
            a.b_name = b.b_name
    WHERE a.b_name = 'A153'--here comes i
    ORDER BY a.r_id
);

--SET search_path TO traj_t1, public;