# Nemo Patches

- NEMO versions prior to v5.0.x need minimal modifications to create a module dedicated to communication with coupled Python scripts. This repository contains patches of modified NEMO sources for versions: 
  	- [v4.0.4](https://forge.ipsl.fr/nemo/browser/NEMO/releases/r4.0/r4.0.4)
	- [v4.0.7](https://forge.ipsl.fr/nemo/browser/NEMO/releases/r4.0/r4.0.7)
	- [v4.2.0](https://forge.nemo-ocean.eu/nemo/nemo/-/releases/4.2.0)
	- [v4.2.1](https://forge.nemo-ocean.eu/nemo/nemo/-/releases/4.2.1)
	- [v4.2.2](https://forge.nemo-ocean.eu/nemo/nemo/-/releases/4.2.2)

## Use a patch 
	- Copy patch sources in the `MY_SRC` directory of a NEMO config
	- Create your Python communication module (more details [here](https://morays-doc.readthedocs.io/en/latest/nemo.api_4.html))
	- Compile NEMO with *key_oasis3* CCP key and OASIS_v5.0 (see this [guide](https://morays-doc.readthedocs.io/en/latest/getting_started.html#base-environment))

## Patch Modifications
  * Architecture: OASIS coupling module `cpl_oasis.F90` was initially managed by SBC module
      - OASIS environnement is now totally managed by NEMO main routines in `nemogcm.F90`
      - Coupling module is independent and can be called by any other module to define coupling variables, send and receive them on demand
      - Now possible to perform exchange of 3D fields (OASIS_V5 needed)

  * Properties of coupling variables are stored in meta-arrays `ssnd` and `srcv` in coupling module
      - Dimension added to the array to sort meta-data between calling modules

  * New modules:        
      - `infmod.F90` : module dedicated to inference models management   /!\ Models must be hard-coded for now /!\
      - `inffld.F90` : memory management for inference models needed fields

<table>
<tr>
<th> Before </th>
<th> After </th>
</tr>
<tr>
<td>
<img width="510" alt="archi_nemo_old" src="https://github.com/alexis-barge/smart-morey/assets/138531178/d68820ef-10b2-459c-afaf-603f2dc4add8">
</td>
<td>
<img width="466" alt="archi_nemo_new" src="https://github.com/alexis-barge/smart-morey/assets/138531178/8e2ac17a-2168-4aa0-9bc9-e666cd66dc5c">
</td>
</tr>
</table>
