(def black "чорный")
(def red "красный")
(def green "зелёный")

(defn log [state message]
  (assoc state :log (conj (:log state) message)))

(defn calc-new-position [distance angle current]
  (let [rads (* angle (/ Math/PI 180.0))
        x (+ (:x current) (* distance (Math/cos rads)))
        y (+ (:y current) (* distance (Math/sin rads)))]
    {:x (Math/round x)
     :y (Math/round y)}))

(defn move [distance state]
  (let [from (:position state)
        to (calc-new-position distance (:angle state) from)
        message (if (= (:carriage state) :down)
                  (str "...Чертим линию из (" (:x from) ", " (:y from)
                       ") в (" (:x to) ", " (:y to)
                       ") используя " (:color state) " цвет.")
                  (str "Передвигаем на " distance
                       " от точки (" (:x from) ", " (:y from) ")"))]
    (assoc (log state message) :position to)))

(defn turn [angle state]
  (assoc (log state (str "Поворачиваем на " angle " градусов"))
         :angle (rem (+ (:angle state) angle) 360)))

(defn carriage-up [state]
  (assoc (log state "Поднимаем каретку") :carriage :up))

(defn carriage-down [state]
  (assoc (log state "Опускаем каретку") :carriage :down))

(defn set-color [color state]
  (assoc (log state (str "Устанавливаем " color " цвет линии."))
         :color color))

(defn set-position [position state]
  (assoc (log state (str "Устанавливаем позицию каретки в ("
                         (:x position) ", " (:y position) ")."))
         :position position))

(defn draw-triangle [size state]
  (->> state
       carriage-down
       (move size)
       (turn 120)
       (move size)
       (turn 120)
       (move size)
       (turn 120)
       carriage-up))

(defn draw-square [size state]
  (->> state
       carriage-down
       (move size)
       (turn 90)
       (move size)
       (turn 90)
       (move size)
       (turn 90)
       (move size)
       (turn 90)
       carriage-up))

(def start-state
  {:position {:x 0 :y 0}
   :angle 0
   :color black
   :carriage :up
   :log []})

(def final-state
  (->> start-state
       (draw-triangle 100)
       (set-position {:x 10 :y 10})
       (set-color red)
       (draw-square 80)))

(doseq [message (:log final-state)]
  (println message))
