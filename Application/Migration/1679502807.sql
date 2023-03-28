CREATE TABLE Worlds (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL
);
CREATE TABLE Quests (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    world_id UUID DEFAULT uuid_generate_v4() NOT NULL
);
ALTER TABLE Quests ADD CONSTRAINT Quests_ref_world_id FOREIGN KEY (world_id) REFERENCES worlds (id) ON DELETE NO ACTION;
