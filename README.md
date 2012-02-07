LomoPack
========

Pacchetto di filtri script-fu per Gimp che simulano l'effetto Lomo.
Testato con gimp 2.6

Installazione
-------------

1. Copiare il file lomopack.scm nella cartella scripts del gimp
	(/home/utente/.gimp-2.6/scripts)
2. Se il programma è in esecuzione, aggiornare la lista dei filtri
	Filtri > Script-Fu > Rinfresca gli scripts

Uso
---

I filtri sono accessibili da: Filtri > Jackroom

### Lomo Border

Permette di aggiungere un effetto di sfocatura o di vignettatura agli
angoli.

### Lomo Burn

Genera una bruciatura casuale del fotogramma. Il filtro varia ogni volta
l'intensità e la posizione della bruciatura

### Lomo Color

Il filtro principale, che permette di modificare i colori sul tipo di una
pellicola ad alta saturazione. 
Nel caso di immagini con ombre molto intense o forte contrasto, è
consigliabile aggiungere luminosità o diminuire il contrasto usando i due
controlli *[PRE]* posti in alto.

Licenza
-------

Copyright (C) 2012  Milo Martini

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
