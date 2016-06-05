/*
 * Testing the pgtraj_v2
 */
INSERT INTO pgtraj.traj_group (g_name) VALUES ('group1');
INSERT INTO pgtraj.traj_group (g_name) VALUES ('group2');

INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'continental',
    'migrating animal',
    1
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'large',
    'wood stork1',
    1
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'large2',
    'wood stork2',
    1
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'medium',
    'sea turtle',
    2
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'small',
    'small animal',
    2
);

-- insert steps
INSERT INTO pgtraj.steps (
    s_id,
    step,
    date,
    dt,
    b_id
) (
    SELECT 
        e.startgid AS s_id,
        e.step_geog AS step,
        e.time AS date,
        e.dt,
        r.b_id
    FROM example_data.steps AS e INNER JOIN pgtraj.bursts AS r
    ON r.b_name = e.id
);



