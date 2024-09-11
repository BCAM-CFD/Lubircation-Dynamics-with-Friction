dir=../particles/
cero=0000
echo > stress.dat

fileb=particles00000
filee=0000.dat

for i in {0..9}
do
    echo $i
    file=$dir$fileb$i$filee
    awk '{print $2, $3, $4}' $file > pos.dat
    awk '{print $5, $6, $7}' $file > vel.dat
    ./biviscous_suspension
    echo $i$cero > step.dat
    awk '{print $2, $3, $4, $5, $6, $7, $8, $9, $10}' output/stress.dat > stress1.dat
    awk '{print $2}' output/shear_rate.dat > shear_rate1.dat
    paste step.dat stress1.dat shear_rate1.dat > stress2.dat
    cat stress.dat stress2.dat > stress3.dat
    mv stress3.dat stress.dat
    rm stress1.dat stress2.dat
    rm shear_rate1.dat
    rm -r output
done


fileb=particles0000
filee=0000.dat

for i in {10..99}
do
    echo $i
    file=$dir$fileb$i$filee
    awk '{print $2, $3, $4}' $file > pos.dat
    awk '{print $5, $6, $7}' $file > vel.dat
    ./biviscous_suspension
    echo $i$cero > step.dat
    awk '{print $2, $3, $4, $5, $6, $7, $8, $9, $10}' output/stress.dat > stress1.dat
    awk '{print $2}' output/shear_rate.dat > shear_rate1.dat
    paste step.dat stress1.dat shear_rate1.dat > stress2.dat
    cat stress.dat stress2.dat > stress3.dat
    mv stress3.dat stress.dat
    rm stress1.dat stress2.dat
    rm shear_rate1.dat
    rm -r output
done

fileb=particles000
filee=0000.dat

for i in {100..999}
do
    echo $i
    file=$dir$fileb$i$filee
    awk '{print $2, $3, $4}' $file > pos.dat
    awk '{print $5, $6, $7}' $file > vel.dat
    ./biviscous_suspension
    echo $i$cero > step.dat
    awk '{print $2, $3, $4, $5, $6, $7, $8, $9, $10}' output/stress.dat > stress1.dat
    awk '{print $2}' output/shear_rate.dat > shear_rate1.dat
    paste step.dat stress1.dat shear_rate1.dat > stress2.dat
    cat stress.dat stress2.dat > stress3.dat
    mv stress3.dat stress.dat
    rm stress1.dat stress2.dat
    rm shear_rate1.dat
    rm -r output
done

fileb=particles00
filee=0000.dat

for i in {1000..1000}
do
    echo $i
    file=$dir$fileb$i$filee
    awk '{print $2, $3, $4}' $file > pos.dat
    awk '{print $5, $6, $7}' $file > vel.dat
    ./biviscous_suspension
    echo $i$cero > step.dat
    awk '{print $2, $3, $4, $5, $6, $7, $8, $9, $10}' output/stress.dat > stress1.dat
    awk '{print $2}' output/shear_rate.dat > shear_rate1.dat
    paste step.dat stress1.dat shear_rate1.dat > stress2.dat
    cat stress.dat stress2.dat > stress3.dat
    mv stress3.dat stress.dat
    rm stress1.dat stress2.dat
    rm shear_rate1.dat
    rm -r output
done
