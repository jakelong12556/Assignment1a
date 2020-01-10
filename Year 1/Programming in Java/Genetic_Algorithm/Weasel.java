package com.bham.pij.assignments.a2a.Weasel;

import java.util.Scanner;
import java.util.ArrayList;
import java.lang.Object;
import java.util.Random;
import java.util.*;

public class Weasel extends GAApplication {//class
    private String weaselStr;
    private Random value;
    private final int binaryLimit = 67;


    public Weasel (String weaselStr) {
        this.weaselStr = weaselStr;
        No_population = 100;
        No_Chromosomes = weaselStr.length();
        No_Parent = 50;
        crossChance = 0.8;
        mutateChance = 0.005;
        value = new Random();
        newPopulation();
    }

    public static void main(String[] args) {
        String WEASEL_GOAL_1 = "And ten thousand peoploids split into small tribes, " +
                "coveting the highest of the sterile skyscrapers, " +
                "like packs of dogs assaulting the glass fronts of Love-Me Avenue.";
        GAApplication gap = new Weasel(WEASEL_GOAL_1);
        int gens = 0;
        double fitness = Double.MAX_VALUE;
        String best = null;
        do {
            gap.run();
            System.out.println("\n");

            best = gap.getBest().toString();
            gens++;
            fitness = gap.getBest().getFitness();
            System.out.println(best);
            System.out.println(gens);
            System.out.println("\n");
        }while (!(best.equals(WEASEL_GOAL_1)) && gens < 1000);

    }

    @Override
    public void run() {
        mutationOperation();//mutate population

        setPopulationFit();
        sortPopulationFitLv();
        setPopulationFit();

        crossover();//perform crossover

        setPopulationFit();
        sortPopulationFitLv();
        setPopulationFit();

        //printPopulationAndFitness();

    }

    @Override
    public void newPopulation() { //create population of individual from

        for (int i = 0; i < No_population; i++) {
            String chromosome;
            Individual individuals = new Individual(binaryLimit, No_Chromosomes);
            individuals.newChromosome();
            individuals.setFitnessWeasel(weaselStr);
            population.add(individuals);
        }
    }

    @Override
    public void printPopulationAndFitness() {
       String tempstring = "";
       String tempstring1 = "";

       for (int i = population.size() - 10; i < population.size(); i++) {

           Individual temp = population.get(i);
           tempstring = temp.toString();
           temp.setFitnessWeasel(weaselStr);
           System.out.println(tempstring + " " + temp.getFitness());
           tempstring = "";
       }

    }

    @Override
    public void mutationOperation() { //mutate population
        for (int i = 0; i < No_population; i++) {
            Individual geneMutate = population.get(i);
            geneMutate.mutateGeneWeasel(mutateChance, weaselStr);
        }
    }


    @Override
    public void sortPopulationFitLv() {
        Collections.sort(population);
    }

    @Override
    public void setPopulationFit() {
        for (int i = 0; i < No_population; i++) {
            Individual setPopulationFit = population.get(i);
            setPopulationFit.setFitnessWeasel(weaselStr);
        }
    }

    public void crossover() {//perform crossover and make the new children
        double isCross = value.nextDouble();

        ArrayList<Integer> randomPosition = new ArrayList<>();

        for (int j = 0; j < No_Parent; j++) {
            randomPosition.add(j);
        }

        Collections.shuffle(randomPosition);

        for (int i = 0; i < No_Parent; i += 2) {
            if (isCross <= crossChance){
                String leftHandDad, rightHandDad, leftHandMom, rightHandMom, childOne, childTwo;

                int crossoverPoint = value.nextInt(No_Chromosomes);

                Individual father = new Individual(population.get(population.size() - randomPosition.get(i) - 1));
                Individual mother = new Individual(population.get(population.size() - randomPosition.get(i + 1) - 1));

                Individual child1 = new Individual(binaryLimit, No_Chromosomes);
                Individual child2 = new Individual(binaryLimit, No_Chromosomes);


                String fatherString = father.toString();
                String motherString = mother.toString();

                leftHandDad = fatherString.substring(0,crossoverPoint);
                rightHandDad = fatherString.substring(crossoverPoint);
                leftHandMom = motherString.substring(0,crossoverPoint);
                rightHandMom = motherString.substring(crossoverPoint);

                childOne = leftHandDad + rightHandMom;
                childTwo = rightHandDad + leftHandMom;

                child1.setGenes(childOne);
                child2.setGenes(childTwo);

                population.set(i, child1);
                population.set(i + 1, child2);


            }

        }
    }

}
