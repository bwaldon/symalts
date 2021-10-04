module load R
module load gcc
# sbatch -p hns --time=3:00:00 --mem=8G --wrap='Rscript select.r "alternative"'
# sbatch -p hns --time=3:00:00 --mem=8G --wrap='Rscript select.r "exh-only"'
# sbatch -p hns --time=3:00:00 --mem=8G --wrap='Rscript select.r "exh-clausal"'
# sbatch -p hns --time=3:00:00 --mem=8G --wrap='Rscript select.r "exh-subclausal"'
sbatch -p hns --time=1:30:00 --mem=12G --wrap="Rscript btwncond_select.r 'Some'"
sbatch -p hns --time=1:30:00 --mem=12G --wrap="Rscript btwncond_select.r 'Number'"
sbatch -p hns --time=1:30:00 --mem=12G --wrap="Rscript btwncond_select.r 'Ad-hoc'"