/*
 * Insert trajectories
 */

-- insert animal names
INSERT INTO pgtraj.animals (name) VALUES ('wood stork1');
INSERT INTO pgtraj.animals (name) VALUES ('wood stork2');
INSERT INTO pgtraj.animals (name) VALUES ('sea turtle');
INSERT INTO pgtraj.animals (name) VALUES ('small animal');
INSERT INTO pgtraj.animals (name) VALUES ('migrating animal');

-- insert trajectory names and match them to animal names
INSERT INTO pgtraj.trajectories (
    name,
    animal_id
) (
    SELECT 
        DISTINCT rel.id,
        a.id
    FROM 
        example_data.relocations_sorted AS rel
    JOIN pgtraj.animals AS a ON (
        rel.id = 'continental' AND a.name = 'migrating animal'
        ) or (
        rel.id = 'large' AND a.name = 'wood stork1'
        ) or (
        rel.id = 'large2' AND a.name = 'wood stork2'
        ) or (
        rel.id = 'medium' AND a.name = 'sea turtle'
        ) or (
        rel.id = 'small' AND a.name = 'small animal'
        )
);

-- insert steps
INSERT INTO pgtraj.steps (
    id,
    step,
    time,
    dtime,
    length
) (
    SELECT 
        startgid,
        step_geog,
        time,
        dt,
        length_geog
    FROM example_data.steps
);

-- create trajectory-burst-step relations
-- burst_id can be NULL
INSERT INTO pgtraj.t_rel_b_rel_s (
    traj_id,
    step_id
) (
    SELECT 
        t.id,
        s.startgid
    FROM 
        pgtraj.trajectories t
    JOIN example_data.steps s ON s.id = t.name
);

-- create bursts
INSERT INTO pgtraj.bursts (name) VALUES ('w_stork_b1');
INSERT INTO pgtraj.bursts (name) VALUES ('w_stork_b2');
-- then update the relationships
UPDATE pgtraj.t_rel_b_rel_s
SET burst_id = (SELECT id
                FROM pgtraj.bursts
                WHERE name = 'w_stork_b1')
WHERE step_id IN (SELECT s.id
                 FROM pgtraj.steps s, pgtraj.t_rel_b_rel_s t
                 WHERE t.traj_id = 8 AND s.length < 1000
);

UPDATE pgtraj.t_rel_b_rel_s
SET burst_id = (SELECT id
                FROM pgtraj.bursts
                WHERE name = 'w_stork_b2')
WHERE step_id IN (SELECT s.id
                 FROM pgtraj.steps s, pgtraj.t_rel_b_rel_s t
                 WHERE t.traj_id = 8 AND s.length >= 1000
);



/*
 * Example queries
 */
-- query all bursts of animal named 'buksi'
SELECT DISTINCT b."name"
FROM 
    pgtraj.t_rel_b_rel_s r,
    pgtraj.bursts b,
    pgtraj.trajectories t,
    pgtraj.animals a
WHERE
    a."name" = 'wood stork1' AND
    t.animal_id = a.id AND
    t.id = r.traj_id AND
    r.burst_id = b.id;

-- SELECT average step length IN bursts
SELECT b.name, avg(s.length)
FROM
    pgtraj.steps s,
    pgtraj.t_rel_b_rel_s r,
    pgtraj.bursts b
WHERE ((r.burst_id = 1 OR r.burst_id = 2) AND r.step_id = s.id) AND r.burst_id = b.id
GROUP BY b.name;
