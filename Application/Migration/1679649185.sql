CREATE TABLE sessions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    quest_id UUID NOT NULL,
    commands TEXT[] DEFAULT '{}' NOT NULL
);
CREATE INDEX sessions_created_at_index ON sessions (created_at);
CREATE INDEX sessions_quest_id_index ON sessions (quest_id);
ALTER TABLE sessions ADD CONSTRAINT sessions_ref_quest_id FOREIGN KEY (quest_id) REFERENCES quests (id) ON DELETE NO ACTION;
