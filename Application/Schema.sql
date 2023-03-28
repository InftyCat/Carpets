-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE worlds (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    columns INT NOT NULL,
    nodenumber INT NOT NULL,
    allowednodes INT[] DEFAULT '{}' NOT NULL
);
CREATE TABLE quests (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    world_id UUID NOT NULL,
    name TEXT NOT NULL,
    org BOOLEAN DEFAULT true NOT NULL
);
CREATE INDEX quests_world_id_index ON quests (world_id);
CREATE TABLE cells (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    quest_id UUID NOT NULL,
    pos INT NOT NULL,
    assumption BOOLEAN DEFAULT true NOT NULL,
    defin_arr INT[] DEFAULT '{}' NOT NULL,
    info TEXT[] DEFAULT '{}' NOT NULL,
    direction TEXT[] DEFAULT '{}' NOT NULL,
    parent_id UUID DEFAULT NULL
);
CREATE INDEX cells_quest_id_index ON cells (quest_id);
CREATE TABLE sessions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    quest_id UUID NOT NULL,
    commands TEXT[] DEFAULT '{}' NOT NULL
);
CREATE INDEX sessions_created_at_index ON sessions (created_at);
CREATE INDEX sessions_quest_id_index ON sessions (quest_id);
ALTER TABLE cells ADD CONSTRAINT cells_ref_quest_id FOREIGN KEY (quest_id) REFERENCES quests (id) ON DELETE CASCADE;
ALTER TABLE quests ADD CONSTRAINT quests_ref_world_id FOREIGN KEY (world_id) REFERENCES worlds (id) ON DELETE NO ACTION;
ALTER TABLE sessions ADD CONSTRAINT sessions_ref_quest_id FOREIGN KEY (quest_id) REFERENCES quests (id) ON DELETE CASCADE;
