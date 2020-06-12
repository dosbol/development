package discourje.core.graph;

import java.util.*;
import java.util.function.BooleanSupplier;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

@SuppressWarnings("unused")
public class Graphs {

    public static <State, Action, Test extends Predicate<Object> & Supplier<Object>> boolean areBisimilar(Graph<State, Action, Test> g1,
                                                                                                          Graph<State, Action, Test> g2) {
        expandRecursively(g1.getRoots());
        expandRecursively(g2.getRoots());

        var vertices = new LinkedHashSet<Vertex<State, Action, Test>>();
        vertices.addAll(g1.getVertices());
        vertices.addAll(g2.getVertices());

        Function<Graph<State, Action, Test>, Collection<Edge.Label<Action, Test>>> getLabels = g -> {
            var labels = new LinkedHashSet<Edge.Label<Action, Test>>();
            for (Vertex<State, Action, Test> v : g.getVertices()) {
                labels.addAll(v.getLabels());
            }
            return labels;
        };

        var labels = new HashSet<Edge.Label<Action, Test>>();
        labels.addAll(getLabels.apply(g1));
        labels.addAll(getLabels.apply(g2));

        var partition = new HashSet<Set<Vertex<State, Action, Test>>>();
        partition.add(vertices);

        while (true) {
            var partition$prime = new HashSet<Set<Vertex<State, Action, Test>>>();

            for (Set<Vertex<State, Action, Test>> block : partition) {
                var intersection = new HashSet<Vertex<State, Action, Test>>();
                var complement = new HashSet<Vertex<State, Action, Test>>();
                BooleanSupplier isSplit = () -> !intersection.isEmpty() && !complement.isEmpty();

                for (Set<Vertex<State, Action, Test>> splitter : partition) {
                    for (Edge.Label<Action, Test> l : labels) {
                        intersection.clear();
                        complement.clear();

                        for (Vertex<State, Action, Test> v : block) {
                            if (v.getLabels().contains(l) && splitter.containsAll(v.traverseNow(l))) {
                                intersection.add(v);
                            } else {
                                complement.add(v);
                            }
                        }

                        if (isSplit.getAsBoolean()) break;
                    }

                    if (isSplit.getAsBoolean()) break;
                }

                if (!intersection.isEmpty()) {
                    partition$prime.add(intersection);
                }
                if (!complement.isEmpty()) {
                    partition$prime.add(complement);
                }
            }

            if (partition.equals(partition$prime)) {
                break;
            } else {
                partition = partition$prime;
            }
        }

        for (Vertex<State, Action, Test> root1 : g1.getRoots()) {
            for (Vertex<State, Action, Test> root2 : g2.getRoots()) {
                var b = false;
                for (Set<Vertex<State, Action, Test>> block : partition) {
                    b = block.contains(root1) && block.contains(root2);
                    if (b) break;
                }
                if (!b) return false;
            }
        }

        return true;
    }

    public static <State, Action, Test extends Predicate<Object> & Supplier<Object>> void expand(Collection<Vertex<State, Action, Test>> vertices) {
        for (Vertex<?, ?, ?> v : vertices) {
            v.expand();
        }
    }

    public static <State, Action, Test extends Predicate<Object> & Supplier<Object>> void expandRecursively(Collection<Vertex<State, Action, Test>> vertices) {
        for (Vertex<?, ?, ?> v : vertices) {
            v.expandRecursively();
        }
    }

    public static <State, Action, Test extends Predicate<Object> & Supplier<Object>> void expandRecursively(Collection<Vertex<State, Action, Test>> vertices, int depth) {
        for (Vertex<?, ?, ?> v : vertices) {
            v.expandRecursively(depth);
        }
    }

    public static <Action> Boolean traverseEventually(Collection<Vertex<?, Action, ?>> vertices,
                                                      Action a,
                                                      Object o) {
        for (Vertex<?, Action, ?> source : vertices) {
            if (source.traverseEventually(a, o)) {
                return true;
            }
        }
        return false;
    }

    public static <Action> Collection<Vertex<?, ?, ?>> traverseNow(Collection<Vertex<?, Action, ?>> vertices,
                                                                   Action a,
                                                                   Object o) {
        var targets = new LinkedHashSet<Vertex<?, ?, ?>>();
        for (Vertex<?, Action, ?> source : vertices) {
            targets.addAll(source.traverseNow(a, o));
        }
        return targets;
    }
}
