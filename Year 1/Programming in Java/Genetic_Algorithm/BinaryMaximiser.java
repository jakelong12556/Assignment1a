package com.bham.pij.assignments.a2a.BinaryMaximiser;

import java.util.Scanner;
import java.util.ArrayList;
import java.lang.Object;
import java.util.Random;
import java.util.*;

public class BinaryMaximiser extends GAApplication {//class
    private int goal;
    private final int binaryLimit = 2;
    private Random value;

    public static void main(String[] args) {
        int BIN_GOAL_1 = 30;
        GAApplication gap = new BinaryMaximiser(BIN_GOAL_1);
        int gens = 0;
        double fitness = Double.MIN_VALUE;

        do {
            gap.run();
            gens++;
            fitness = gap.getBest().getFitness();
            System.out.println("fitness: " + fitness);
            System.out.println("generation: " + gens);
            //System.out.println("\n");
        } while (fitness != BIN_GOAL_1 && gens < 1000);

    }

    public BinaryMaximiser (int goal) {
        this.goal = goal;
        No_population = 100;
        No_Chromosomes = goal;
        No_Parent = 50;
        crossChance = 0.7;
        mutateChance = 0.001;
        value = new Random();
        newPopulation();
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

        printPopulationAndFitness();

    }

    @Override
    public void newPopulation() { //create population of individual from 0-1
        for (int i = 0; i < No_population; i++) {
            Individual individuals = new Individual(binaryLimit, goal);
            individuals.newChromosome();
            individuals.setFitnessBinary();
            population.add(individuals);
        }
    }

    @Override
    public void setPopulationFit() {
        for (int i = 0; i < No_population; i++) {
            Individual setPopulationFit = population.get(i);
            setPopulationFit.setFitnessBinary();
        }
    }

    @Override
    public void mutationOperation() { //mutate population
        for (int i = 0; i < No_population; i++) {
            Individual geneMutate = population.get(i);
            geneMutate.mutateGenesBinary(mutateChance);
        }
    }


    @Override
    public void sortPopulationFitLv() {
        Collections.sort(population);
    }


    @Override
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

                Individual child1 = new Individual(binaryLimit, goal);
                Individual child2 = new Individual(binaryLimit, goal);


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
