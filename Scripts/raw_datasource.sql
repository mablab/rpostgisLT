/* Mimic raw data source, extended with pgtraj, animal, burst info
 * id = pgtraj
 */
CREATE TABLE example_data.relocations_plus AS
SELECT *
FROM example_data.relocations;

ALTER TABLE example_data.relocations_plus ADD animal text;

UPDATE example_data.relocations_plus
SET animal =    CASE  
                    WHEN id = 'continental' THEN 'migrating animal'
                    WHEN id = 'small' THEN 'small animal'
                    WHEN id = 'large' THEN 'wood stork1'
                    WHEN id = 'large2' THEN 'wood stork2'
                    WHEN id = 'medium' THEN 'sea turtle'
                END;

ALTER TABLE example_data.relocations_plus ADD burst text;

UPDATE example_data.relocations_plus
SET burst =    CASE  
                    WHEN id = 'continental' THEN 'burst1'
                    WHEN id = 'small' THEN 'burst2'
                    WHEN id = 'large' THEN 'burst3'
                    WHEN id = 'large2' THEN 'burst4'
                    WHEN id = 'medium' THEN 'burst5'
                END;
