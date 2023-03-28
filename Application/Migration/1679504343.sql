DROP TABLE worlds;
ALTER TABLE Quests DROP CONSTRAINT quests_ref_world_id;
CREATE TABLE Worlds (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    coloumns INT NOT NULL,
    nodenumber INT NOT NULL,
    allowednodes INT[] DEFAULT '{}' NOT NULL
);
ALTER TABLE Quests ADD CONSTRAINT Quests_ref_world_id FOREIGN KEY (world_id) REFERENCES worlds (id) ON DELETE NO ACTION;
