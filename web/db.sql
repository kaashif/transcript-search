\timing
drop table raw_transcripts;
create table raw_transcripts (series text, season_number int, episode_number int, episode_title text, scene_number int, line_number int, person text, present text, place text, speech text);

\copy raw_transcripts (series, season_number, episode_number, episode_title, scene_number, line_number, person, present, place, speech) from transcripts.tsv

drop table transcripts;
create table transcripts (id serial primary key, series text, season_number int, episode_number int, episode_title text, scene_number int, line_number int, person text, present text, place text, speech text);

insert into transcripts (series, season_number, episode_number, episode_title, scene_number, line_number, person, present, place, speech) (select * from raw_transcripts order by series, season_number, episode_number, scene_number, line_number);
grant select on transcripts to transcripts;

drop table raw_transcripts;
