unit HaunterData;

interface

type
  TGClassWeakness = record
  GClass: Cardinal;
  GWeakness: Cardinal;
end;

type
  TPowerData = record
  family: Cardinal;
  p_type: Cardinal;
  band: Cardinal;
  recharge_time: Cardinal;
  unk: Cardinal;
  unk2: Cardinal;
  name: Cardinal;
  description: Cardinal;
end;

type
  TGParams = record
  attention: Cardinal;
  discipline: Cardinal;
  intelligence: Cardinal;
  training_level: Cardinal;
  unlocked: Cardinal;
  controller_type: Cardinal;
  power_1: Cardinal;
  power_2: Cardinal;
  power_3: Cardinal;
  power_4: Cardinal;
  power_5: Cardinal;
  power_6: Cardinal;
  power_7: Cardinal;
  power_8: Cardinal;
  power_9: Cardinal;
  power_10: Cardinal;
  spower_1_1: Cardinal;
  spower_1_2: Cardinal;
  spower_2_1: Cardinal;
  spower_2_2: Cardinal;
  spower_3_1: Cardinal;
  spower_3_2: Cardinal;
  spower_4_1: Cardinal;
  spower_4_2: Cardinal;
  spower_5_1: Cardinal;
  spower_5_2: Cardinal;
  spower_6_1: Cardinal;
  spower_6_2: Cardinal;
  spower_7_1: Cardinal;
  spower_7_2: Cardinal;
  spower_8_1: Cardinal;
  spower_8_2: Cardinal;
  spower_9_1: Cardinal;
  spower_9_2: Cardinal;
  spower_10_1: Cardinal;
  spower_10_2: Cardinal;
end;

type
  THaunterData = record
  script: string[50];
  model_file: string[50];
  type_script: string[50];
  ghost_script: string[50];
  spawn: string[50];
  link_to_object: string[50];
  camera: string[50];
  anim_id: Cardinal;
  gtype: Cardinal;
  name: Cardinal;
  bio: Cardinal;
  haunter_state: Cardinal;
  parameters: TGParams;
  fetter: Cardinal;
  experience: Cardinal;
  time_gate: Cardinal;
end;

implementation

end.
