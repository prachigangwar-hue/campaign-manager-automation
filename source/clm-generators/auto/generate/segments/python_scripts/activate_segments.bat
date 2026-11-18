@echo off
cd /d D:\products\clm_generators\auto\generate\segments\python_scripts

call D:\products\clm_generators\auto\generate\segments\python_scripts\env\Scripts\activate.bat

python segment_language_state_low_performing_new.py

python segment_coupon_users.py

python segment_fmcg_new.py

python segment_teenz.py

python seg_Lr_App_Active.py

python segment_religion.py

python segment_active_store.py






