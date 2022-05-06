import tensorflow as tf
from tensorflow import keras
from sklearn.metrics import roc_auc_score

class RankingModel:
    
    def __init__(self, config, logger):
        self.config = config
        self.logger = logger
        self.model = None 
        self.learning_rate = self.config.get("learning_rate")
        self.batch_size = self.config.get("batch_size")
        self.n_epochs = self.config.get("n_epochs")

    def check_model(self):
        if self.model is None:
            self.logger.Error("Se debe entrenear el modelo primero")
            exit()
    
    def train(self, X_train, y_train):
        self.logger.Info("Training model")
        self.model = keras.Sequential([
            keras.layers.Dense(6, activation='sigmoid', 
                               input_shape=(X_train.shape[-1],)),
            keras.layers.Dense(1, activation='sigmoid'),])
        self.model.compile(optimizer=tf.keras.optimizers.Adam(),
                           loss=keras.losses.BinaryCrossentropy())
        self.model.fit(X_train, y_train, batch_size=self.batch_size,
                       epochs=self.n_epochs, verbose=2)
        self.logger.Info("Computing metrics")
        y_train_hat = self.model.predict(X_train)
        auc_score = roc_auc_score(y_train, y_train_hat)
        self.logger.Info("Train AUC {}".format(auc_score))
        
    def test(self, X_test, y_test):
        self.check_model()
        y_test_hat = self.model.predict(X_test)
        auc_score = roc_auc_score(y_test, y_test_hat)
        self.logger.Info("Test AUC {}".format(auc_score))
        
    def predict(self, X):
        self.check_model()
        return self.model.predict(X)
    
    def get_weights(self):
        self.check_model()
        return {"W1": self.model.weights[0].numpy().tolist(),
           "b1": self.model.weights[1].numpy().reshape(1, -1).tolist(),
           "W2": self.model.weights[2].numpy().tolist(),
           "b2": self.model.weights[3].numpy().reshape(1, -1).tolist()}
