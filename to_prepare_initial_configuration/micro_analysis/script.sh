dir=../Montecarlo/16_8_64/
dir=../Montecarlo/phi0_4_32_32_64/

file_b=pos00
file_e=00000.dat
for i in {300..341..1}
do
    echo $i
    file=$dir$file_b$i$file_e
    awk '{print $2, $3, $4}' $file > pos.dat
    ./micro_program
done
