ALTER TABLE quests ADD COLUMN world_id UUID NOT NULL;
CREATE INDEX quests_world_id_index ON quests (world_id);
ALTER TABLE quests ADD CONSTRAINT quests_ref_world_id FOREIGN KEY (world_id) REFERENCES worlds (id) ON DELETE NO ACTION;
